open Aws.BaseTypes

type t =
  | Submitted
  | Active
  | Cancelled
  | Failed
  | Cancelled_running
  | Cancelled_terminating
  | Modifying

let str_to_t =
  [ "modifying", Modifying
  ; "cancelled_terminating", Cancelled_terminating
  ; "cancelled_running", Cancelled_running
  ; "failed", Failed
  ; "cancelled", Cancelled
  ; "active", Active
  ; "submitted", Submitted
  ]

let t_to_str =
  [ Modifying, "modifying"
  ; Cancelled_terminating, "cancelled_terminating"
  ; Cancelled_running, "cancelled_running"
  ; Failed, "failed"
  ; Cancelled, "cancelled"
  ; Active, "active"
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
