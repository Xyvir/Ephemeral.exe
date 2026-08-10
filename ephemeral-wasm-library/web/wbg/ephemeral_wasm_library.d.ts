/* tslint:disable */
/* eslint-disable */
/**
 * The `ReadableStreamType` enum.
 *
 * *This API requires the following crate features to be activated: `ReadableStreamType`*
 */

export type ReadableStreamType = "bytes";

/**
 * A browser-side ephemeral client: owns an iroh endpoint and submits
 * jobs to cluster compute nodes by their EndpointTicket.
 */
export class EphemeralClient {
    private constructor();
    free(): void;
    [Symbol.dispose](): void;
    /**
     * Create a client endpoint.
     *
     * `secret_key_hex` (optional) pins a persistent node identity;
     * `relay_url` (optional) overrides the public n0 relays with a
     * self-hosted relay (e.g. `"https://relay.example.com."`).
     */
    static create(secret_key_hex?: string | null, relay_url?: string | null): Promise<EphemeralClient>;
    /**
     * Discover the cluster around a seed node — no user-supplied ticket
     * needed beyond the bootstrap config.
     *
     * Dial `seed_ticket`, complete the ``hello`` handshake, and resolve
     * with a JSON string describing the seed itself plus any peers its
     * hello carried:
     *
     * ```json
     * {"seed":{"node_id":"…","ticket":"…","images":["…"],"rtt_ms":42},
     *  "peers":[{"node_id":"…","ticket":"…","images":[],"rtt_ms":42}]}
     * ```
     *
     * Peers carry dialable EndpointTickets, so one seed is enough to
     * learn and reach the whole cluster.
     */
    discover(seed_ticket: string): Promise<any>;
    /**
     * A serialized EndpointTicket others can dial this client with.
     */
    make_ticket(): string;
    /**
     * This client's node id (hex).
     */
    node_id(): string;
    /**
     * Submit a job to the compute node described by `ticket`.
     *
     * `document_blob` is a base64-encoded UTF-8 Markdown document (same
     * contract as the REST `RunRequest`). `on_event` is called once per
     * wire frame with a JSON string: `{"type":"job_log","channel":...,
     * "data":"<base64>"}`, `{"type":"job_done",...}`, or
     * `{"type":"error","message":...}`. The returned promise resolves
     * when the job terminates and rejects if the exchange fails.
     */
    submit_job(ticket: string, document_blob: string, timeout: number, on_event: Function): Promise<any>;
}

export class IntoUnderlyingByteSource {
    private constructor();
    free(): void;
    [Symbol.dispose](): void;
    cancel(): void;
    pull(controller: ReadableByteStreamController): Promise<any>;
    start(controller: ReadableByteStreamController): void;
    readonly autoAllocateChunkSize: number;
    readonly type: ReadableStreamType;
}

export class IntoUnderlyingSink {
    private constructor();
    free(): void;
    [Symbol.dispose](): void;
    abort(reason: any): Promise<any>;
    close(): Promise<any>;
    write(chunk: any): Promise<any>;
}

export class IntoUnderlyingSource {
    private constructor();
    free(): void;
    [Symbol.dispose](): void;
    cancel(): void;
    pull(controller: ReadableStreamDefaultController): Promise<any>;
}

/**
 * Decode a base64 string (used by the SPA for job_log data).
 */
export function base64_decode(data: string): Uint8Array;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly __wbg_ephemeralclient_free: (a: number, b: number) => void;
    readonly base64_decode: (a: number, b: number) => [number, number, number, number];
    readonly ephemeralclient_create: (a: number, b: number, c: number, d: number) => any;
    readonly ephemeralclient_discover: (a: number, b: number, c: number) => any;
    readonly ephemeralclient_make_ticket: (a: number) => [number, number, number, number];
    readonly ephemeralclient_node_id: (a: number) => [number, number];
    readonly ephemeralclient_submit_job: (a: number, b: number, c: number, d: number, e: number, f: number, g: any) => any;
    readonly __wbg_intounderlyingbytesource_free: (a: number, b: number) => void;
    readonly intounderlyingbytesource_autoAllocateChunkSize: (a: number) => number;
    readonly intounderlyingbytesource_cancel: (a: number) => void;
    readonly intounderlyingbytesource_pull: (a: number, b: any) => any;
    readonly intounderlyingbytesource_start: (a: number, b: any) => void;
    readonly intounderlyingbytesource_type: (a: number) => number;
    readonly __wbg_intounderlyingsink_free: (a: number, b: number) => void;
    readonly intounderlyingsink_abort: (a: number, b: any) => any;
    readonly intounderlyingsink_close: (a: number) => any;
    readonly intounderlyingsink_write: (a: number, b: any) => any;
    readonly __wbg_intounderlyingsource_free: (a: number, b: number) => void;
    readonly intounderlyingsource_cancel: (a: number) => void;
    readonly intounderlyingsource_pull: (a: number, b: any) => any;
    readonly ring_core_0_17_14__bn_mul_mont: (a: number, b: number, c: number, d: number, e: number, f: number) => void;
    readonly wasm_bindgen_18c2c14b3664a5fa___convert__closures_____invoke___wasm_bindgen_18c2c14b3664a5fa___JsValue__core_9b3796e30d99ddb7___result__Result_____wasm_bindgen_18c2c14b3664a5fa___JsError___true_: (a: number, b: number, c: any) => [number, number];
    readonly wasm_bindgen_18c2c14b3664a5fa___convert__closures_____invoke___js_sys_9b8caca0fb7b6e5e___Function_fn_wasm_bindgen_18c2c14b3664a5fa___JsValue_____wasm_bindgen_18c2c14b3664a5fa___sys__Undefined___js_sys_9b8caca0fb7b6e5e___Function_fn_wasm_bindgen_18c2c14b3664a5fa___JsValue_____wasm_bindgen_18c2c14b3664a5fa___sys__Undefined_______true_: (a: number, b: number, c: any, d: any) => void;
    readonly wasm_bindgen_18c2c14b3664a5fa___convert__closures_____invoke___wasm_bindgen_18c2c14b3664a5fa___JsValue______true_: (a: number, b: number, c: any) => void;
    readonly wasm_bindgen_18c2c14b3664a5fa___convert__closures_____invoke___web_sys_da9ba819b8c2bae8___features__gen_CloseEvent__CloseEvent______true_: (a: number, b: number, c: any) => void;
    readonly wasm_bindgen_18c2c14b3664a5fa___convert__closures_____invoke___web_sys_da9ba819b8c2bae8___features__gen_MessageEvent__MessageEvent______true_: (a: number, b: number, c: any) => void;
    readonly wasm_bindgen_18c2c14b3664a5fa___convert__closures_____invoke_______true_: (a: number, b: number) => void;
    readonly wasm_bindgen_18c2c14b3664a5fa___convert__closures_____invoke_______true__1_: (a: number, b: number) => void;
    readonly wasm_bindgen_18c2c14b3664a5fa___convert__closures_____invoke_______true__2_: (a: number, b: number) => void;
    readonly wasm_bindgen_18c2c14b3664a5fa___convert__closures_____invoke_______true__3_: (a: number, b: number) => void;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __wbindgen_exn_store: (a: number) => void;
    readonly __externref_table_alloc: () => number;
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __wbindgen_destroy_closure: (a: number, b: number) => void;
    readonly __externref_table_dealloc: (a: number) => void;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
 * Instantiates the given `module`, which can either be bytes or
 * a precompiled `WebAssembly.Module`.
 *
 * @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
 *
 * @returns {InitOutput}
 */
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
 * If `module_or_path` is {RequestInfo} or {URL}, makes a request and
 * for everything else, calls `WebAssembly.instantiate` directly.
 *
 * @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
 *
 * @returns {Promise<InitOutput>}
 */
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
