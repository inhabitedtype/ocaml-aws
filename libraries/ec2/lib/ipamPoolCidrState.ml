open Aws.BaseTypes

type t =
  | Pending_provision
  | Provisioned
  | Failed_provision
  | Pending_deprovision
  | Deprovisioned
  | Failed_deprovision
  | Pending_import
  | Failed_import

let str_to_t =
  [ "failed-import", Failed_import
  ; "pending-import", Pending_import
  ; "failed-deprovision", Failed_deprovision
  ; "deprovisioned", Deprovisioned
  ; "pending-deprovision", Pending_deprovision
  ; "failed-provision", Failed_provision
  ; "provisioned", Provisioned
  ; "pending-provision", Pending_provision
  ]

let t_to_str =
  [ Failed_import, "failed-import"
  ; Pending_import, "pending-import"
  ; Failed_deprovision, "failed-deprovision"
  ; Deprovisioned, "deprovisioned"
  ; Pending_deprovision, "pending-deprovision"
  ; Failed_provision, "failed-provision"
  ; Provisioned, "provisioned"
  ; Pending_provision, "pending-provision"
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
