open Aws.BaseTypes

type t =
  | UNREACHABLE
  | CONNECTION_FAILED
  | AUTH_FAILURE
  | PENDING_PROXY_CAPACITY
  | INVALID_REPLICATION_STATE
  | PROMOTED

let str_to_t =
  [ "PROMOTED", PROMOTED
  ; "INVALID_REPLICATION_STATE", INVALID_REPLICATION_STATE
  ; "PENDING_PROXY_CAPACITY", PENDING_PROXY_CAPACITY
  ; "AUTH_FAILURE", AUTH_FAILURE
  ; "CONNECTION_FAILED", CONNECTION_FAILED
  ; "UNREACHABLE", UNREACHABLE
  ]

let t_to_str =
  [ PROMOTED, "PROMOTED"
  ; INVALID_REPLICATION_STATE, "INVALID_REPLICATION_STATE"
  ; PENDING_PROXY_CAPACITY, "PENDING_PROXY_CAPACITY"
  ; AUTH_FAILURE, "AUTH_FAILURE"
  ; CONNECTION_FAILED, "CONNECTION_FAILED"
  ; UNREACHABLE, "UNREACHABLE"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
