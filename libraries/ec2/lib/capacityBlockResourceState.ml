open Aws.BaseTypes

type t =
  | Active
  | Expired
  | Unavailable
  | Cancelled
  | Failed
  | Scheduled
  | Payment_pending
  | Payment_failed

let str_to_t =
  [ "payment-failed", Payment_failed
  ; "payment-pending", Payment_pending
  ; "scheduled", Scheduled
  ; "failed", Failed
  ; "cancelled", Cancelled
  ; "unavailable", Unavailable
  ; "expired", Expired
  ; "active", Active
  ]

let t_to_str =
  [ Payment_failed, "payment-failed"
  ; Payment_pending, "payment-pending"
  ; Scheduled, "scheduled"
  ; Failed, "failed"
  ; Cancelled, "cancelled"
  ; Unavailable, "unavailable"
  ; Expired, "expired"
  ; Active, "active"
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
