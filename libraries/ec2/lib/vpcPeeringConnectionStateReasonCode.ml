open Aws.BaseTypes

type t =
  | Initiating_request
  | Pending_acceptance
  | Active
  | Deleted
  | Rejected
  | Failed
  | Expired
  | Provisioning
  | Deleting

let str_to_t =
  [ "deleting", Deleting
  ; "provisioning", Provisioning
  ; "expired", Expired
  ; "failed", Failed
  ; "rejected", Rejected
  ; "deleted", Deleted
  ; "active", Active
  ; "pending-acceptance", Pending_acceptance
  ; "initiating-request", Initiating_request
  ]

let t_to_str =
  [ Deleting, "deleting"
  ; Provisioning, "provisioning"
  ; Expired, "expired"
  ; Failed, "failed"
  ; Rejected, "rejected"
  ; Deleted, "deleted"
  ; Active, "active"
  ; Pending_acceptance, "pending-acceptance"
  ; Initiating_request, "initiating-request"
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
