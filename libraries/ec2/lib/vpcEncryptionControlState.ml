open Aws.BaseTypes

type t =
  | Enforce_in_progress
  | Monitor_in_progress
  | Enforce_failed
  | Monitor_failed
  | Deleting
  | Deleted
  | Available
  | Creating
  | Delete_failed

let str_to_t =
  [ "delete-failed", Delete_failed
  ; "creating", Creating
  ; "available", Available
  ; "deleted", Deleted
  ; "deleting", Deleting
  ; "monitor-failed", Monitor_failed
  ; "enforce-failed", Enforce_failed
  ; "monitor-in-progress", Monitor_in_progress
  ; "enforce-in-progress", Enforce_in_progress
  ]

let t_to_str =
  [ Delete_failed, "delete-failed"
  ; Creating, "creating"
  ; Available, "available"
  ; Deleted, "deleted"
  ; Deleting, "deleting"
  ; Monitor_failed, "monitor-failed"
  ; Enforce_failed, "enforce-failed"
  ; Monitor_in_progress, "monitor-in-progress"
  ; Enforce_in_progress, "enforce-in-progress"
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
