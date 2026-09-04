"""
Mesh image pull — warm images from peers, verified against the registry.

When a distributed node needs a container image it does not have cached,
it currently pulls from the registry (slow, hammers the registry). If a
peer advertises the image warm, the node can instead assemble it from
that peer's blobs transferred over iroh — but the peer's word is not
enough for a supply-chain-safe unattended mesh, so the *registry manifest*
is the trust anchor:

1. fetch the image manifest from the registry itself (small, HTTPS,
   Bearer-token auth handled) — it names the config and each layer by
   ``sha256:`` digest and size;
2. fetch each blob from the warm peer over iroh and verify its sha256
   against the manifest digest (a tampered layer can never pass);
3. assemble an OCI layout on disk and ``podman load`` it (Podman
   re-verifies digests on load), then tag it to the canonical ref.

Any failure at any step falls back to the registry pull — mesh pull never
raises, it returns False and the caller keeps the existing behavior.

The serving side exports images with ``podman save --format oci-dir`` on
demand (cached by the node), which reproduces the exact content-addressed
blobs the registry published, so blob digests line up.

Wire protocol: one bi-stream per blob, ``blob_request`` then a sequence
of ``blob_chunk`` frames (base64, size-capped) ending in ``blob_done``
or an ``error`` frame — same shape as the job protocol.
"""
from __future__ import annotations

import asyncio
import base64
import hashlib
import json
import logging
import platform
import re
import shutil
import subprocess
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from pathlib import Path

logger = logging.getLogger(__name__)

#: Raw bytes per ``blob_chunk`` frame (base64 inflates 4/3; 6 MiB -> ~8 MiB
#: payload, comfortably under the 32 MiB frame guard).
BLOB_CHUNK_SIZE = 6 * 1024 * 1024

#: Registry manifest media types we accept (Docker v2 and OCI).
_MANIFEST_ACCEPT = ", ".join(
    [
        "application/vnd.docker.distribution.manifest.v2+json",
        "application/vnd.docker.distribution.manifest.list.v2+json",
        "application/vnd.oci.image.manifest.v1+json",
        "application/vnd.oci.image.index.v1+json",
    ]
)

_DIGEST_RE = re.compile(r"^sha256:[0-9a-f]{64}$")


class ImagePullError(Exception):
    """A mesh pull step failed (missing peer, bad blob, verify mismatch)."""


# --- image references -----------------------------------------------------

def parse_image_ref(ref: str) -> tuple[str, str, str]:
    """
    Split an image reference into ``(registry, repository, tag)``.

    ``docker.io/library/alpine:latest`` -> ``("registry-1.docker.io",
    "library/alpine", "latest")``; ``alpine`` -> docker.io + the implicit
    ``library/`` prefix; digest refs keep the digest as the tag.
    """
    ref = ref.strip()
    if not ref:
        raise ImagePullError("empty image ref")
    registry = "docker.io"
    rest = ref
    if "/" in ref:
        first, _, tail = ref.partition("/")
        if "." in first or ":" in first or first == "localhost":
            registry, rest = first, tail
        else:
            rest = ref
    if ":" in rest.rsplit("/", 1)[-1]:
        repo, _, tag = rest.rpartition(":")
    else:
        repo, tag = rest, "latest"
    if registry == "docker.io" and "/" not in repo:
        repo = "library/" + repo
    registry = "registry-1.docker.io" if registry == "docker.io" else registry
    if not repo or not tag:
        raise ImagePullError(f"cannot parse image ref: {ref!r}")
    return registry, repo, tag


def host_arch() -> str:
    """This host's container platform architecture (linux always)."""
    machine = platform.machine().lower()
    return {"x86_64": "amd64", "amd64": "amd64", "aarch64": "arm64", "arm64": "arm64"}.get(
        machine, "amd64"
    )


def _normalize_architecture(value: object) -> str | None:
    """Normalize common OCI/Podman architecture spellings."""
    if not value:
        return None
    value = str(value).strip().lower()
    return {
        "x86_64": "amd64",
        "amd64": "amd64",
        "aarch64": "arm64",
        "arm64": "arm64",
    }.get(value, value)


# --- registry manifest (the trust anchor) --------------------------------

def _registry_get(
    registry: str, path: str, headers: dict, timeout: float = 20.0
) -> tuple[int, dict, bytes]:
    """GET ``https://<registry>/v2/<path>`` returning (status, headers, body)."""
    url = f"https://{registry}/v2/{path}"
    req = urllib.request.Request(url, headers=headers)
    try:
        with urllib.request.urlopen(req, timeout=timeout) as res:
            return res.status, dict(res.headers), res.read()
    except urllib.error.HTTPError as e:
        return e.code, dict(e.headers), e.read()


def _fetch_token(challenge: str, registry: str, repo: str, timeout: float = 20.0) -> str:
    """
    Resolve a ``WWW-Authenticate: Bearer realm=...,service=...,scope=...``
    challenge into a pull token. Handles Docker Hub (``token``) and
    GitHub/Quay (``access_token``) response shapes.
    """
    m = re.search(r'realm="([^"]+)"', challenge)
    if not m:
        raise ImagePullError(f"registry {registry}: unsupported auth challenge")
    params = {"service": "", "scope": f"repository:{repo}:pull"}
    sm = re.search(r'service="([^"]*)"', challenge)
    if sm:
        params["service"] = sm.group(1)
    sc = re.search(r'scope="([^"]*)"', challenge)
    if sc:
        params["scope"] = sc.group(1)
    url = f"{m.group(1)}?{urllib.parse.urlencode(params)}"
    try:
        with urllib.request.urlopen(url, timeout=timeout) as res:
            payload = json.loads(res.read().decode("utf-8"))
    except Exception as e:
        raise ImagePullError(f"registry {registry}: token fetch failed: {e}") from e
    token = payload.get("token") or payload.get("access_token")
    if not token:
        raise ImagePullError(f"registry {registry}: no token in auth response")
    return str(token)


def fetch_manifest(
    ref: str,
    *,
    timeout: float = 20.0,
    arch: str | None = None,
) -> tuple[dict, str, bytes]:
    """
    Fetch the *platform* manifest for ``ref`` from its registry.

    Returns ``(manifest, manifest_digest, manifest_bytes)``. Resolves an
    index/manifest-list down to the host platform (linux/<arch>) by a
    second authenticated manifest fetch by digest. This is the trust
    anchor: every blob digest and size the mesh puller relies on comes
    from here, over HTTPS.
    """
    registry, repo, tag = parse_image_ref(ref)
    arch = arch or host_arch()
    token: str | None = None

    def _get() -> tuple[int, dict, bytes]:
        headers = {"Accept": _MANIFEST_ACCEPT}
        if token:
            headers["Authorization"] = f"Bearer {token}"
        return _registry_get(registry, f"{repo}/manifests/{tag}", headers, timeout)

    status, headers, body = _get()
    if status == 401:
        token = _fetch_token(headers.get("WWW-Authenticate", ""), registry, repo, timeout)
        status, _headers, body = _get()
    if status == 404:
        raise ImagePullError(f"registry {registry}: manifest for {ref!r} not found")
    if status != 200:
        raise ImagePullError(f"registry {registry}: manifest fetch failed: HTTP {status}")

    manifest = json.loads(body.decode("utf-8"))
    digest = "sha256:" + hashlib.sha256(body).hexdigest()
    # If this is an index, resolve the platform manifest by its digest.
    if "manifests" in manifest and isinstance(manifest.get("manifests"), list):
        picked = None
        for entry in manifest["manifests"]:
            plat = entry.get("platform") or {}
            if plat.get("os") == "linux" and plat.get("architecture") == arch:
                picked = entry
                break
        if picked is None:
            # Never fall back to another CPU architecture. Podman may emit a
            # warning and cache an unusable image, which later fails with
            # `/usr/bin/sh: Exec format error` on the host.
            raise ImagePullError(
                f"registry {registry}: no linux/{arch} manifest for {ref!r}"
            )
        tag = picked["digest"]  # fetch the platform manifest by digest
        status, _headers, body = _get()
        if status != 200:
            raise ImagePullError(
                f"registry {registry}: platform manifest fetch failed: HTTP {status}"
            )
        manifest = json.loads(body.decode("utf-8"))
        digest = "sha256:" + hashlib.sha256(body).hexdigest()
    if "layers" not in manifest or "config" not in manifest:
        raise ImagePullError(f"registry {registry}: manifest for {ref!r} has no layers")
    return manifest, digest, body


@dataclass
class BlobSpec:
    """One content-addressed blob the manifest requires."""

    digest: str
    size: int


def manifest_blobs(manifest: dict) -> list[BlobSpec]:
    """The config + layers a platform manifest requires (config first)."""
    config = manifest.get("config") or {}
    layers = manifest.get("layers") or []
    specs: list[BlobSpec] = []
    if config.get("digest"):
        specs.append(BlobSpec(str(config["digest"]), int(config.get("size") or 0)))
    for layer in layers:
        if layer.get("digest"):
            specs.append(BlobSpec(str(layer["digest"]), int(layer.get("size") or 0)))
    return specs


def verify_blob(path: Path, digest: str) -> bool:
    """Whether ``path`` hashes to exactly ``digest`` (sha256:...)."""
    if not _DIGEST_RE.match(digest):
        return False
    hasher = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            hasher.update(chunk)
    return hasher.hexdigest() == digest[len("sha256:"):]


# --- OCI layout assembly + podman ----------------------------------------

def assemble_oci_layout(
    root: Path,
    ref: str,
    manifest: dict,
    manifest_digest: str,
    manifest_bytes: bytes,
    blobs: dict[str, Path],
) -> Path:
    """
    Build an OCI image layout in ``root`` from verified blob files.

    ``blobs`` maps ``sha256:<digest>`` -> a file whose content hashes to
    that digest. Copies the config + layers + manifest into
    ``root/blobs/sha256/`` and writes ``index.json`` (with the canonical
    ``ref`` as the image name) + ``oci-layout``. Returns ``root``.
    Raises :class:`ImagePullError` when a required blob is missing or its
    content does not verify.
    """
    blobs_dir = root / "blobs" / "sha256"
    blobs_dir.mkdir(parents=True, exist_ok=True)

    def _stage(digest: str, src: Path) -> None:
        if not _DIGEST_RE.match(digest):
            raise ImagePullError(f"bad blob digest: {digest!r}")
        if not src.exists():
            raise ImagePullError(f"blob {digest[:16]}... missing locally")
        if not verify_blob(src, digest):
            raise ImagePullError(f"blob {digest[:16]}... failed sha256 verification")
        shutil.copyfile(src, blobs_dir / digest)

    for spec in manifest_blobs(manifest):
        src = blobs.get(spec.digest)
        if src is None:
            raise ImagePullError(f"required blob {spec.digest[:16]}... was not fetched")
        _stage(spec.digest, src)
    (blobs_dir / manifest_digest).write_bytes(manifest_bytes)

    index = {
        "schemaVersion": 2,
        "mediaType": "application/vnd.oci.image.index.v1+json",
        "manifests": [
            {
                "mediaType": manifest.get("mediaType")
                or "application/vnd.docker.distribution.manifest.v2+json",
                "digest": manifest_digest,
                "size": len(manifest_bytes),
                "annotations": {"org.opencontainers.image.ref.name": ref},
            }
        ],
    }
    (root / "index.json").write_text(json.dumps(index), encoding="utf-8")
    (root / "oci-layout").write_text(
        json.dumps({"imageLayoutVersion": "1.0.0"}), encoding="utf-8"
    )
    return root


def _layout_config_digest(root: Path) -> str:
    """Config blob digest recorded in the layout's index (for re-tagging)."""
    try:
        index = json.loads((root / "index.json").read_text(encoding="utf-8"))
        manifest_ref = index["manifests"][0]["digest"]
        manifest = json.loads(
            (root / "blobs" / "sha256" / manifest_ref).read_text(encoding="utf-8")
        )
        return str((manifest.get("config") or {}).get("digest", ""))
    except Exception:
        return ""


def load_oci_layout(root: Path, ref: str) -> bool:
    """
    ``podman load`` the OCI layout in ``root`` and ensure ``ref`` exists.

    Archives the layout (oci-archive format), loads it, and re-tags to the
    canonical ref if Podman named it differently. Returns False on any
    Podman failure (the caller keeps the registry-pull fallback).
    """
    archive = root.parent / f"{root.name}.tar.gz"
    try:
        subprocess.run(
            ["tar", "-C", str(root), "-czf", str(archive), "."],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        subprocess.run(
            ["podman", "load", "-i", str(archive)],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        try:
            subprocess.run(
                ["podman", "image", "exists", ref],
                check=True,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            return True
        except subprocess.CalledProcessError:
            pass
        # Podman may have named it differently — re-tag by the config
        # digest (the loaded image's ID; unique, so the prefix resolves).
        config_digest = _layout_config_digest(root)
        if config_digest:
            subprocess.run(
                ["podman", "tag", config_digest, ref],
                check=True,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            return True
        return False
    except Exception as e:
        logger.warning("podman load of %s failed: %s", ref, e)
        return False
    finally:
        try:
            archive.unlink(missing_ok=True)
        except Exception:  # pragma: no cover - best effort
            pass


def export_oci_dir(image: str, out: Path) -> Path:
    """
    Export a locally-cached image as an OCI layout dir (serving side).

    ``podman save --format oci-dir`` reproduces the exact content-addressed
    blobs the registry published (layers are stored digest-verified), so
    blob digests line up with the registry manifest. Raises when Podman
    cannot save (image missing / Podman down) — the node then refuses the
    blob request.
    """
    out.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        ["podman", "save", "--format", "oci-dir", "--output", str(out), image],
        check=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return out


# --- the puller ----------------------------------------------------------

class MeshImagePuller:
    """
    Assemble an image from a warm peer's blobs, verified against the
    registry manifest. Every failure returns False — callers keep the
    registry-pull fallback; this never raises.
    """

    def __init__(
        self,
        node,
        *,
        manifest_fetcher=fetch_manifest,
        loader=load_oci_layout,
    ) -> None:
        self.node = node
        self._manifest_fetcher = manifest_fetcher
        self._loader = loader

    async def pull(self, image: str, *, preferred_peer=None) -> bool:
        """Try to make ``image`` warm from a peer. Returns success."""
        try:
            manifest, mdigest, mbytes = await asyncio.to_thread(
                self._manifest_fetcher, image
            )
        except Exception as e:
            logger.info("mesh pull of %s: registry manifest unavailable (%s)", image, e)
            return False

        # Mesh blobs must come from a peer running the same native platform
        # as this node. A warm amd64 image is not a valid source for an
        # arm64 pull (and vice versa), even when the repository/tag matches.
        requested_platform = {"os": "linux", "architecture": host_arch()}
        if preferred_peer is not None:
            peer_platform = getattr(preferred_peer, "platform", None)
            if peer_platform and (
                peer_platform.get("os", "linux") != requested_platform["os"]
                or _normalize_architecture(peer_platform.get("architecture"))
                != requested_platform["architecture"]
            ):
                preferred_peer = None
        peer = preferred_peer or self.node.peer_for_images(
            [image], platform=requested_platform
        )
        if peer is None:
            logger.info("mesh pull of %s: no same-platform peer advertises it warm", image)
            return False

        root = Path(tempfile.mkdtemp(prefix="ephemeral-mesh-layout-"))
        try:
            # Blobs land in a staging dir first; assemble_oci_layout copies
            # them into blobs/sha256/ (re-verifying each one) and writes the
            # index — fetch and stage stay separate so nothing is ever
            # copied onto itself.
            src_dir = root / "src"
            src_dir.mkdir(parents=True, exist_ok=True)
            fetched: dict[str, Path] = {}
            for spec in manifest_blobs(manifest):
                dest = src_dir / spec.digest
                ok = await self._fetch_one(peer, image, spec, dest)
                if not ok:
                    return False
                fetched[spec.digest] = dest
            assemble_oci_layout(root, image, manifest, mdigest, mbytes, fetched)
            ok = await asyncio.to_thread(self._loader, root, image)
            if not ok:
                logger.info("mesh pull of %s: podman load failed", image)
            return ok
        except Exception as e:
            logger.warning("mesh pull of %s failed: %s", image, e)
            return False
        finally:
            shutil.rmtree(root, ignore_errors=True)

    async def _fetch_one(self, peer, image: str, spec: BlobSpec, dest: Path) -> bool:
        try:
            await self.node.fetch_blob(peer, image, spec.digest, spec.size, dest)
        except Exception as e:
            logger.info(
                "mesh pull of %s: blob %s from %s failed: %s",
                image,
                spec.digest[:16],
                getattr(peer, "node_id", "?")[:8],
                e,
            )
            return False
        if not verify_blob(dest, spec.digest):
            logger.info(
                "mesh pull of %s: blob %s failed sha256 verification",
                image,
                spec.digest[:16],
            )
            return False
        return True


__all__ = [
    "BLOB_CHUNK_SIZE",
    "BlobSpec",
    "ImagePullError",
    "MeshImagePuller",
    "assemble_oci_layout",
    "export_oci_dir",
    "fetch_manifest",
    "host_arch",
    "load_oci_layout",
    "manifest_blobs",
    "parse_image_ref",
    "verify_blob",
]
