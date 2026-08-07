open Aws.BaseTypes

type t =
  | Submitted
  | Modifying
  | Active
  | Partially_fulfilled
  | Expiring
  | Expired
  | Cancelling
  | Cancelled
  | Failed

let str_to_t =
  [ "failed", Failed
  ; "cancelled", Cancelled
  ; "cancelling", Cancelling
  ; "expired", Expired
  ; "expiring", Expiring
  ; "partially_fulfilled", Partially_fulfilled
  ; "active", Active
  ; "modifying", Modifying
  ; "submitted", Submitted
  ]

let t_to_str =
  [ Failed, "failed"
  ; Cancelled, "cancelled"
  ; Cancelling, "cancelling"
  ; Expired, "expired"
  ; Expiring, "expiring"
  ; Partially_fulfilled, "partially_fulfilled"
  ; Active, "active"
  ; Modifying, "modifying"
  ; Submitted, "submitted"
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
