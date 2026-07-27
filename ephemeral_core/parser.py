"""
Ephemeral Markdown parser: codeblock extraction and runtime resolution.

Extracted verbatim from the original ephemeral.py (lines 179-380).
No GUI, HTTP, or platform-specific code exists in this module.
"""
import re
import shlex

from .config import LANG_MAP, NETWORK_FLAGS, NO_CHAIN_FLAGS


def strip_ansi_codes(text: str) -> str:
    """Remove ANSI escape sequences from text."""
    ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
    return ansi_escape.sub('', text)


def strip_shebang(text: str) -> str:
    """Remove the first line if it's a shebang (#!) directive."""
    if not text: return text
    if text.lstrip().startswith("#!"):
        parts = text.split('\n', 1)
        if len(parts) > 1:
            return parts[1]
        return ""
    return text


def __shlex_join(split_command: list[str]) -> str:
    """Compatibility wrapper for shlex.join (added in Python 3.8)."""
    if hasattr(shlex, 'join'):
        return shlex.join(split_command)
    return ' '.join(shlex.quote(arg) for arg in split_command)


def resolve_runtime_config(header_line: str) -> dict | None:
    """
    Parse a codeblock header line into a runtime configuration dictionary.
    
    Resolves the language, image, command, version overrides, network flags,
    and chaining flags from the header tokens.
    
    Returns None if the header is empty or unresolvable.
    """
    if not header_line: return None
    try: tokens = shlex.split(header_line)
    except: tokens = header_line.split() 
    if not tokens: return None

    # 1. Detect Network Flags
    network_enabled = False
    chain_enabled = True
    cleaned_tokens = []
    
    for token in tokens:
        if token.lower() in NETWORK_FLAGS:
            network_enabled = True
        elif token.lower() in NO_CHAIN_FLAGS:
            chain_enabled = False
        else:
            cleaned_tokens.append(token)
            
    if not cleaned_tokens: return None
    
    # 2. Parse Language
    base_lang_input = cleaned_tokens[0].lower()
    overrides = {}
    
    for token in cleaned_tokens[1:]:
        if '=' in token:
            key, val = token.split('=', 1)
            overrides[key.lower()] = val

    base_lang = base_lang_input
    version = None
    match = re.match(r"^([a-z0-9\+\#]+)(?:[:\-](\d+(?:\.\d+)*))?$", base_lang_input)
    if match:
        base_lang = match.group(1)
        version = match.group(2) 
        
    if base_lang in LANG_MAP:
        resolved = LANG_MAP[base_lang]
        if isinstance(resolved, str):
            base_lang = resolved
            if base_lang in LANG_MAP and isinstance(LANG_MAP[base_lang], str):
                 base_lang = LANG_MAP[base_lang]
        elif isinstance(resolved, dict):
            pass

    config = None
    if base_lang in LANG_MAP and isinstance(LANG_MAP[base_lang], dict):
        config = LANG_MAP[base_lang].copy()
    
    if not config:
        if 'image' in overrides: config = {'image': '', 'cmd': []}
        else:
            image_tag = f"{base_lang_input}" if ':' in base_lang_input else f"{base_lang_input}:latest"
            config = {'image': image_tag, 'cmd': [base_lang, '-']}

    if version and config and 'image' not in overrides:
        original_image = config.get('image', '')
        if ':' in original_image:
            repo = original_image.split(':')[0]
            config['image'] = f"{repo}:{version}"
        else:
            config['image'] = f"{original_image}:{version}"

    if 'image' in overrides: config['image'] = overrides['image']
    if 'cmd' in overrides: config['cmd'] = shlex.split(overrides['cmd'])
    if 'entrypoint' in overrides: config['entrypoint'] = overrides['entrypoint']
    
    config['allow_network'] = network_enabled
    config['allow_chain'] = chain_enabled
    return config


def parse_codeblocks(content: str) -> list[dict]:
    """
    Parse Markdown content into classified codeblocks.
    
    Returns a list of dicts, each either:
      - {'type': 'seed', 'name': str, 'content': str, 'is_b64': bool}
      - {'type': 'code', 'header': str, 'content': str, 'config': dict}
    
    Supports fenced codeblocks (```) and shebang (#!) syntax.
    """
    blocks = []
    if not content or not content.strip():
        return blocks

    # Strip any lines starting with 4 or more backticks (to ignore markdown documentation wrappers)
    content = re.sub(r"(?m)^\s*`{4,}.*$\n?", "", content)

    pattern = r"```(.*?)\n(.*?)```"
    matches = list(re.finditer(pattern, content, re.DOTALL))
    if matches:
        for match in matches:
            header = match.group(1).strip() if match.group(1) else ""
            block_content = match.group(2)
            
            block_lines = block_content.splitlines()
            if block_lines:
                first_line = block_lines[0].strip()
                if first_line.startswith("#!"):
                    shebang_val = first_line.lstrip("#!").strip()
                    block_content = strip_shebang(block_content)
                    
                    # If shebang exists, it OVERWRITES the markdown header
                    header = shebang_val
                
            blocks.append({'header': header, 'content': block_content})
    else:
        parts = re.split(r"(?m)^#![ \t]*", content)
        if len(parts) > 1:
            for i, part in enumerate(parts):
                if i == 0 and not part.strip():
                    continue
                if i == 0 and part.strip():
                    blocks.append({'header': '', 'content': part})
                    continue
                part_lines = part.split('\n', 1)
                header = part_lines[0].strip()
                block_content = part_lines[1] if len(part_lines) > 1 else ""
                blocks.append({'header': header, 'content': block_content})
        else:
            blocks.append({'header': '', 'content': content})

    classified = []
    for b in blocks:
        header = b['header']
        tokens = header.split() if header else []
        is_seed = False
        is_b64 = False
        if tokens:
            first_token = tokens[0]
            if re.search(r'\.[a-zA-Z0-9]{1,8}$', first_token) and ':' not in first_token and first_token.lower() not in LANG_MAP:
                is_seed = True
            elif first_token.lower() == 'file' and len(tokens) > 1:
                first_token = tokens[1]
                is_seed = True
                
            if is_seed and 'b64' in [t.lower() for t in tokens]:
                is_b64 = True
                
        if is_seed:
            classified.append({'type': 'seed', 'name': first_token, 'content': b['content'], 'is_b64': is_b64})
        else:
            config = resolve_runtime_config(header)
            classified.append({'type': 'code', 'header': header, 'content': b['content'], 'config': config})
            
    return classified
