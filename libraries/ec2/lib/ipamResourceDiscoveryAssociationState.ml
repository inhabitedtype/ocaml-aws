open Aws.BaseTypes

type t =
  | Associate_in_progress
  | Associate_complete
  | Associate_failed
  | Disassociate_in_progress
  | Disassociate_complete
  | Disassociate_failed
  | Isolate_in_progress
  | Isolate_complete
  | Restore_in_progress

let str_to_t =
  [ "restore-in-progress", Restore_in_progress
  ; "isolate-complete", Isolate_complete
  ; "isolate-in-progress", Isolate_in_progress
  ; "disassociate-failed", Disassociate_failed
  ; "disassociate-complete", Disassociate_complete
  ; "disassociate-in-progress", Disassociate_in_progress
  ; "associate-failed", Associate_failed
  ; "associate-complete", Associate_complete
  ; "associate-in-progress", Associate_in_progress
  ]

let t_to_str =
  [ Restore_in_progress, "restore-in-progress"
  ; Isolate_complete, "isolate-complete"
  ; Isolate_in_progress, "isolate-in-progress"
  ; Disassociate_failed, "disassociate-failed"
  ; Disassociate_complete, "disassociate-complete"
  ; Disassociate_in_progress, "disassociate-in-progress"
  ; Associate_failed, "associate-failed"
  ; Associate_complete, "associate-complete"
  ; Associate_in_progress, "associate-in-progress"
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
