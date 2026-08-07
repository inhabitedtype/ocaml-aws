open Aws.BaseTypes

type t =
  | Instance_stuck_in_state
  | Unresponsive
  | Not_accepting_credentials
  | Password_not_available
  | Performance_network
  | Performance_instance_store
  | Performance_ebs_volume
  | Performance_other
  | Other

let str_to_t =
  [ "other", Other
  ; "performance-other", Performance_other
  ; "performance-ebs-volume", Performance_ebs_volume
  ; "performance-instance-store", Performance_instance_store
  ; "performance-network", Performance_network
  ; "password-not-available", Password_not_available
  ; "not-accepting-credentials", Not_accepting_credentials
  ; "unresponsive", Unresponsive
  ; "instance-stuck-in-state", Instance_stuck_in_state
  ]

let t_to_str =
  [ Other, "other"
  ; Performance_other, "performance-other"
  ; Performance_ebs_volume, "performance-ebs-volume"
  ; Performance_instance_store, "performance-instance-store"
  ; Performance_network, "performance-network"
  ; Password_not_available, "password-not-available"
  ; Not_accepting_credentials, "not-accepting-credentials"
  ; Unresponsive, "unresponsive"
  ; Instance_stuck_in_state, "instance-stuck-in-state"
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
