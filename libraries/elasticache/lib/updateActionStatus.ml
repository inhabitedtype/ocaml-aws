open Aws.BaseTypes

type t =
  | Not_applied
  | Waiting_to_start
  | In_progress
  | Stopping
  | Stopped
  | Complete
  | Scheduling
  | Scheduled
  | Not_applicable

let str_to_t =
  [ "not-applicable", Not_applicable
  ; "scheduled", Scheduled
  ; "scheduling", Scheduling
  ; "complete", Complete
  ; "stopped", Stopped
  ; "stopping", Stopping
  ; "in-progress", In_progress
  ; "waiting-to-start", Waiting_to_start
  ; "not-applied", Not_applied
  ]

let t_to_str =
  [ Not_applicable, "not-applicable"
  ; Scheduled, "scheduled"
  ; Scheduling, "scheduling"
  ; Complete, "complete"
  ; Stopped, "stopped"
  ; Stopping, "stopping"
  ; In_progress, "in-progress"
  ; Waiting_to_start, "waiting-to-start"
  ; Not_applied, "not-applied"
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
