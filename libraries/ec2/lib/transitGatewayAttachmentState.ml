open Aws.BaseTypes

type t =
  | Initiating
  | InitiatingRequest
  | PendingAcceptance
  | RollingBack
  | Pending
  | Available
  | Modifying
  | Deleting
  | Deleted
  | Failed
  | Rejected
  | Rejecting
  | Failing

let str_to_t =
  [ "failing", Failing
  ; "rejecting", Rejecting
  ; "rejected", Rejected
  ; "failed", Failed
  ; "deleted", Deleted
  ; "deleting", Deleting
  ; "modifying", Modifying
  ; "available", Available
  ; "pending", Pending
  ; "rollingBack", RollingBack
  ; "pendingAcceptance", PendingAcceptance
  ; "initiatingRequest", InitiatingRequest
  ; "initiating", Initiating
  ]

let t_to_str =
  [ Failing, "failing"
  ; Rejecting, "rejecting"
  ; Rejected, "rejected"
  ; Failed, "failed"
  ; Deleted, "deleted"
  ; Deleting, "deleting"
  ; Modifying, "modifying"
  ; Available, "available"
  ; Pending, "pending"
  ; RollingBack, "rollingBack"
  ; PendingAcceptance, "pendingAcceptance"
  ; InitiatingRequest, "initiatingRequest"
  ; Initiating, "initiating"
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
