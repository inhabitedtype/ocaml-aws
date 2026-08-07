open Aws.BaseTypes

type t =
  | Submitted
  | Active
  | Deleted
  | Failed
  | Deleted_running
  | Deleted_terminating
  | Modifying

let str_to_t =
  [ "modifying", Modifying
  ; "deleted_terminating", Deleted_terminating
  ; "deleted_running", Deleted_running
  ; "failed", Failed
  ; "deleted", Deleted
  ; "active", Active
  ; "submitted", Submitted
  ]

let t_to_str =
  [ Modifying, "modifying"
  ; Deleted_terminating, "deleted_terminating"
  ; Deleted_running, "deleted_running"
  ; Failed, "failed"
  ; Deleted, "deleted"
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
