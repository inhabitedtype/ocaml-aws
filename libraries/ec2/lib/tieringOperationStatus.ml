open Aws.BaseTypes

type t =
  | Archival_in_progress
  | Archival_completed
  | Archival_failed
  | Temporary_restore_in_progress
  | Temporary_restore_completed
  | Temporary_restore_failed
  | Permanent_restore_in_progress
  | Permanent_restore_completed
  | Permanent_restore_failed

let str_to_t =
  [ "permanent-restore-failed", Permanent_restore_failed
  ; "permanent-restore-completed", Permanent_restore_completed
  ; "permanent-restore-in-progress", Permanent_restore_in_progress
  ; "temporary-restore-failed", Temporary_restore_failed
  ; "temporary-restore-completed", Temporary_restore_completed
  ; "temporary-restore-in-progress", Temporary_restore_in_progress
  ; "archival-failed", Archival_failed
  ; "archival-completed", Archival_completed
  ; "archival-in-progress", Archival_in_progress
  ]

let t_to_str =
  [ Permanent_restore_failed, "permanent-restore-failed"
  ; Permanent_restore_completed, "permanent-restore-completed"
  ; Permanent_restore_in_progress, "permanent-restore-in-progress"
  ; Temporary_restore_failed, "temporary-restore-failed"
  ; Temporary_restore_completed, "temporary-restore-completed"
  ; Temporary_restore_in_progress, "temporary-restore-in-progress"
  ; Archival_failed, "archival-failed"
  ; Archival_completed, "archival-completed"
  ; Archival_in_progress, "archival-in-progress"
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
