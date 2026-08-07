open Aws.BaseTypes

type t =
  | PendingAcceptance
  | Associating
  | Associated
  | Disassociating
  | Disassociated
  | Rejected
  | Failed

let str_to_t =
  [ "failed", Failed
  ; "rejected", Rejected
  ; "disassociated", Disassociated
  ; "disassociating", Disassociating
  ; "associated", Associated
  ; "associating", Associating
  ; "pendingAcceptance", PendingAcceptance
  ]

let t_to_str =
  [ Failed, "failed"
  ; Rejected, "rejected"
  ; Disassociated, "disassociated"
  ; Disassociating, "disassociating"
  ; Associated, "associated"
  ; Associating, "associating"
  ; PendingAcceptance, "pendingAcceptance"
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
