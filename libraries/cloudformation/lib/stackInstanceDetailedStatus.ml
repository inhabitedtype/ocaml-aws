open Aws.BaseTypes

type t =
  | PENDING
  | RUNNING
  | SUCCEEDED
  | FAILED
  | CANCELLED
  | INOPERABLE
  | SKIPPED_SUSPENDED_ACCOUNT
  | FAILED_IMPORT

let str_to_t =
  [ "FAILED_IMPORT", FAILED_IMPORT
  ; "SKIPPED_SUSPENDED_ACCOUNT", SKIPPED_SUSPENDED_ACCOUNT
  ; "INOPERABLE", INOPERABLE
  ; "CANCELLED", CANCELLED
  ; "FAILED", FAILED
  ; "SUCCEEDED", SUCCEEDED
  ; "RUNNING", RUNNING
  ; "PENDING", PENDING
  ]

let t_to_str =
  [ FAILED_IMPORT, "FAILED_IMPORT"
  ; SKIPPED_SUSPENDED_ACCOUNT, "SKIPPED_SUSPENDED_ACCOUNT"
  ; INOPERABLE, "INOPERABLE"
  ; CANCELLED, "CANCELLED"
  ; FAILED, "FAILED"
  ; SUCCEEDED, "SUCCEEDED"
  ; RUNNING, "RUNNING"
  ; PENDING, "PENDING"
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
