open Aws.BaseTypes

type t =
  | SUCCESS
  | FAILED
  | FAILED_SIGNING_FILE
  | PENDING
  | RESOURCE_NOT_FOUND
  | ACCESS_DENIED
  | ACCESS_DENIED_SIGNING_FILE
  | CANCELLED
  | UNKNOWN

let str_to_t =
  [ "UNKNOWN", UNKNOWN
  ; "CANCELLED", CANCELLED
  ; "ACCESS_DENIED_SIGNING_FILE", ACCESS_DENIED_SIGNING_FILE
  ; "ACCESS_DENIED", ACCESS_DENIED
  ; "RESOURCE_NOT_FOUND", RESOURCE_NOT_FOUND
  ; "PENDING", PENDING
  ; "FAILED_SIGNING_FILE", FAILED_SIGNING_FILE
  ; "FAILED", FAILED
  ; "SUCCESS", SUCCESS
  ]

let t_to_str =
  [ UNKNOWN, "UNKNOWN"
  ; CANCELLED, "CANCELLED"
  ; ACCESS_DENIED_SIGNING_FILE, "ACCESS_DENIED_SIGNING_FILE"
  ; ACCESS_DENIED, "ACCESS_DENIED"
  ; RESOURCE_NOT_FOUND, "RESOURCE_NOT_FOUND"
  ; PENDING, "PENDING"
  ; FAILED_SIGNING_FILE, "FAILED_SIGNING_FILE"
  ; FAILED, "FAILED"
  ; SUCCESS, "SUCCESS"
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
