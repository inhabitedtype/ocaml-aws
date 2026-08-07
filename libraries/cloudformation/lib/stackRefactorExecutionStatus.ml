open Aws.BaseTypes

type t =
  | UNAVAILABLE
  | AVAILABLE
  | OBSOLETE
  | EXECUTE_IN_PROGRESS
  | EXECUTE_COMPLETE
  | EXECUTE_FAILED
  | ROLLBACK_IN_PROGRESS
  | ROLLBACK_COMPLETE
  | ROLLBACK_FAILED

let str_to_t =
  [ "ROLLBACK_FAILED", ROLLBACK_FAILED
  ; "ROLLBACK_COMPLETE", ROLLBACK_COMPLETE
  ; "ROLLBACK_IN_PROGRESS", ROLLBACK_IN_PROGRESS
  ; "EXECUTE_FAILED", EXECUTE_FAILED
  ; "EXECUTE_COMPLETE", EXECUTE_COMPLETE
  ; "EXECUTE_IN_PROGRESS", EXECUTE_IN_PROGRESS
  ; "OBSOLETE", OBSOLETE
  ; "AVAILABLE", AVAILABLE
  ; "UNAVAILABLE", UNAVAILABLE
  ]

let t_to_str =
  [ ROLLBACK_FAILED, "ROLLBACK_FAILED"
  ; ROLLBACK_COMPLETE, "ROLLBACK_COMPLETE"
  ; ROLLBACK_IN_PROGRESS, "ROLLBACK_IN_PROGRESS"
  ; EXECUTE_FAILED, "EXECUTE_FAILED"
  ; EXECUTE_COMPLETE, "EXECUTE_COMPLETE"
  ; EXECUTE_IN_PROGRESS, "EXECUTE_IN_PROGRESS"
  ; OBSOLETE, "OBSOLETE"
  ; AVAILABLE, "AVAILABLE"
  ; UNAVAILABLE, "UNAVAILABLE"
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
