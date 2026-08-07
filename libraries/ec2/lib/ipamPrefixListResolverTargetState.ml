open Aws.BaseTypes

type t =
  | Create_in_progress
  | Create_complete
  | Create_failed
  | Modify_in_progress
  | Modify_complete
  | Modify_failed
  | Sync_in_progress
  | Sync_complete
  | Sync_failed
  | Delete_in_progress
  | Delete_complete
  | Delete_failed
  | Isolate_in_progress
  | Isolate_complete
  | Restore_in_progress

let str_to_t =
  [ "restore-in-progress", Restore_in_progress
  ; "isolate-complete", Isolate_complete
  ; "isolate-in-progress", Isolate_in_progress
  ; "delete-failed", Delete_failed
  ; "delete-complete", Delete_complete
  ; "delete-in-progress", Delete_in_progress
  ; "sync-failed", Sync_failed
  ; "sync-complete", Sync_complete
  ; "sync-in-progress", Sync_in_progress
  ; "modify-failed", Modify_failed
  ; "modify-complete", Modify_complete
  ; "modify-in-progress", Modify_in_progress
  ; "create-failed", Create_failed
  ; "create-complete", Create_complete
  ; "create-in-progress", Create_in_progress
  ]

let t_to_str =
  [ Restore_in_progress, "restore-in-progress"
  ; Isolate_complete, "isolate-complete"
  ; Isolate_in_progress, "isolate-in-progress"
  ; Delete_failed, "delete-failed"
  ; Delete_complete, "delete-complete"
  ; Delete_in_progress, "delete-in-progress"
  ; Sync_failed, "sync-failed"
  ; Sync_complete, "sync-complete"
  ; Sync_in_progress, "sync-in-progress"
  ; Modify_failed, "modify-failed"
  ; Modify_complete, "modify-complete"
  ; Modify_in_progress, "modify-in-progress"
  ; Create_failed, "create-failed"
  ; Create_complete, "create-complete"
  ; Create_in_progress, "create-in-progress"
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
