open Aws.BaseTypes

type t =
  | Active
  | Expired
  | Cancelled
  | Scheduled
  | Pending
  | Failed
  | Delayed
  | Unsupported
  | Payment_pending
  | Payment_failed
  | Retired

let str_to_t =
  [ "retired", Retired
  ; "payment-failed", Payment_failed
  ; "payment-pending", Payment_pending
  ; "unsupported", Unsupported
  ; "delayed", Delayed
  ; "failed", Failed
  ; "pending", Pending
  ; "scheduled", Scheduled
  ; "cancelled", Cancelled
  ; "expired", Expired
  ; "active", Active
  ]

let t_to_str =
  [ Retired, "retired"
  ; Payment_failed, "payment-failed"
  ; Payment_pending, "payment-pending"
  ; Unsupported, "unsupported"
  ; Delayed, "delayed"
  ; Failed, "failed"
  ; Pending, "pending"
  ; Scheduled, "scheduled"
  ; Cancelled, "cancelled"
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
