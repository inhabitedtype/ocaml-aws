open Aws.BaseTypes

type t =
  | Deprovisioned
  | Failed_deprovision
  | Failed_provision
  | Pending_deprovision
  | Pending_provision
  | Provisioned

let str_to_t =
  [ "provisioned", Provisioned
  ; "pending-provision", Pending_provision
  ; "pending-deprovision", Pending_deprovision
  ; "failed-provision", Failed_provision
  ; "failed-deprovision", Failed_deprovision
  ; "deprovisioned", Deprovisioned
  ]

let t_to_str =
  [ Provisioned, "provisioned"
  ; Pending_provision, "pending-provision"
  ; Pending_deprovision, "pending-deprovision"
  ; Failed_provision, "failed-provision"
  ; Failed_deprovision, "failed-deprovision"
  ; Deprovisioned, "deprovisioned"
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
