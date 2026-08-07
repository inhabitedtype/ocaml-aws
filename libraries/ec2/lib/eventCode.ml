open Aws.BaseTypes

type t =
  | Instance_reboot
  | System_reboot
  | System_maintenance
  | Instance_retirement
  | Instance_stop

let str_to_t =
  [ "instance-stop", Instance_stop
  ; "instance-retirement", Instance_retirement
  ; "system-maintenance", System_maintenance
  ; "system-reboot", System_reboot
  ; "instance-reboot", Instance_reboot
  ]

let t_to_str =
  [ Instance_stop, "instance-stop"
  ; Instance_retirement, "instance-retirement"
  ; System_maintenance, "system-maintenance"
  ; System_reboot, "system-reboot"
  ; Instance_reboot, "instance-reboot"
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
