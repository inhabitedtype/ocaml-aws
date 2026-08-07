open Aws.BaseTypes

type t =
  | Payment_pending
  | Active
  | Payment_failed
  | Retired
  | Queued
  | Queued_deleted

let str_to_t =
  [ "queued-deleted", Queued_deleted
  ; "queued", Queued
  ; "retired", Retired
  ; "payment-failed", Payment_failed
  ; "active", Active
  ; "payment-pending", Payment_pending
  ]

let t_to_str =
  [ Queued_deleted, "queued-deleted"
  ; Queued, "queued"
  ; Retired, "retired"
  ; Payment_failed, "payment-failed"
  ; Active, "active"
  ; Payment_pending, "payment-pending"
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
