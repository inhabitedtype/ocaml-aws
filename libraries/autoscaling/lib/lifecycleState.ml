open Aws.BaseTypes

type t =
  | Pending
  | Pending_Wait
  | Pending_Proceed
  | Quarantined
  | InService
  | Terminating
  | Terminating_Wait
  | Terminating_Proceed
  | Terminating_Retained
  | Terminated
  | Detaching
  | Detached
  | EnteringStandby
  | Standby
  | ReplacingRootVolume
  | ReplacingRootVolume_Wait
  | ReplacingRootVolume_Proceed
  | RootVolumeReplaced
  | Warmed_Pending
  | Warmed_Pending_Wait
  | Warmed_Pending_Proceed
  | Warmed_Pending_Retained
  | Warmed_Terminating
  | Warmed_Terminating_Wait
  | Warmed_Terminating_Proceed
  | Warmed_Terminating_Retained
  | Warmed_Terminated
  | Warmed_Stopped
  | Warmed_Running
  | Warmed_Hibernated

let str_to_t =
  [ "Warmed:Hibernated", Warmed_Hibernated
  ; "Warmed:Running", Warmed_Running
  ; "Warmed:Stopped", Warmed_Stopped
  ; "Warmed:Terminated", Warmed_Terminated
  ; "Warmed:Terminating:Retained", Warmed_Terminating_Retained
  ; "Warmed:Terminating:Proceed", Warmed_Terminating_Proceed
  ; "Warmed:Terminating:Wait", Warmed_Terminating_Wait
  ; "Warmed:Terminating", Warmed_Terminating
  ; "Warmed:Pending:Retained", Warmed_Pending_Retained
  ; "Warmed:Pending:Proceed", Warmed_Pending_Proceed
  ; "Warmed:Pending:Wait", Warmed_Pending_Wait
  ; "Warmed:Pending", Warmed_Pending
  ; "RootVolumeReplaced", RootVolumeReplaced
  ; "ReplacingRootVolume:Proceed", ReplacingRootVolume_Proceed
  ; "ReplacingRootVolume:Wait", ReplacingRootVolume_Wait
  ; "ReplacingRootVolume", ReplacingRootVolume
  ; "Standby", Standby
  ; "EnteringStandby", EnteringStandby
  ; "Detached", Detached
  ; "Detaching", Detaching
  ; "Terminated", Terminated
  ; "Terminating:Retained", Terminating_Retained
  ; "Terminating:Proceed", Terminating_Proceed
  ; "Terminating:Wait", Terminating_Wait
  ; "Terminating", Terminating
  ; "InService", InService
  ; "Quarantined", Quarantined
  ; "Pending:Proceed", Pending_Proceed
  ; "Pending:Wait", Pending_Wait
  ; "Pending", Pending
  ]

let t_to_str =
  [ Warmed_Hibernated, "Warmed:Hibernated"
  ; Warmed_Running, "Warmed:Running"
  ; Warmed_Stopped, "Warmed:Stopped"
  ; Warmed_Terminated, "Warmed:Terminated"
  ; Warmed_Terminating_Retained, "Warmed:Terminating:Retained"
  ; Warmed_Terminating_Proceed, "Warmed:Terminating:Proceed"
  ; Warmed_Terminating_Wait, "Warmed:Terminating:Wait"
  ; Warmed_Terminating, "Warmed:Terminating"
  ; Warmed_Pending_Retained, "Warmed:Pending:Retained"
  ; Warmed_Pending_Proceed, "Warmed:Pending:Proceed"
  ; Warmed_Pending_Wait, "Warmed:Pending:Wait"
  ; Warmed_Pending, "Warmed:Pending"
  ; RootVolumeReplaced, "RootVolumeReplaced"
  ; ReplacingRootVolume_Proceed, "ReplacingRootVolume:Proceed"
  ; ReplacingRootVolume_Wait, "ReplacingRootVolume:Wait"
  ; ReplacingRootVolume, "ReplacingRootVolume"
  ; Standby, "Standby"
  ; EnteringStandby, "EnteringStandby"
  ; Detached, "Detached"
  ; Detaching, "Detaching"
  ; Terminated, "Terminated"
  ; Terminating_Retained, "Terminating:Retained"
  ; Terminating_Proceed, "Terminating:Proceed"
  ; Terminating_Wait, "Terminating:Wait"
  ; Terminating, "Terminating"
  ; InService, "InService"
  ; Quarantined, "Quarantined"
  ; Pending_Proceed, "Pending:Proceed"
  ; Pending_Wait, "Pending:Wait"
  ; Pending, "Pending"
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
