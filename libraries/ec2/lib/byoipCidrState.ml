open Aws.BaseTypes

type t =
  | Advertised
  | Deprovisioned
  | Failed_deprovision
  | Failed_provision
  | Pending_advertising
  | Pending_deprovision
  | Pending_provision
  | Pending_withdrawal
  | Provisioned
  | Provisioned_not_publicly_advertisable

let str_to_t =
  [ "provisioned-not-publicly-advertisable", Provisioned_not_publicly_advertisable
  ; "provisioned", Provisioned
  ; "pending-withdrawal", Pending_withdrawal
  ; "pending-provision", Pending_provision
  ; "pending-deprovision", Pending_deprovision
  ; "pending-advertising", Pending_advertising
  ; "failed-provision", Failed_provision
  ; "failed-deprovision", Failed_deprovision
  ; "deprovisioned", Deprovisioned
  ; "advertised", Advertised
  ]

let t_to_str =
  [ Provisioned_not_publicly_advertisable, "provisioned-not-publicly-advertisable"
  ; Provisioned, "provisioned"
  ; Pending_withdrawal, "pending-withdrawal"
  ; Pending_provision, "pending-provision"
  ; Pending_deprovision, "pending-deprovision"
  ; Pending_advertising, "pending-advertising"
  ; Failed_provision, "failed-provision"
  ; Failed_deprovision, "failed-deprovision"
  ; Deprovisioned, "deprovisioned"
  ; Advertised, "advertised"
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
