open Aws.BaseTypes

type t =
  | Disassociated
  | Failed_disassociation
  | Failed_association
  | Pending_disassociation
  | Pending_association
  | Associated

let str_to_t =
  [ "associated", Associated
  ; "pending-association", Pending_association
  ; "pending-disassociation", Pending_disassociation
  ; "failed-association", Failed_association
  ; "failed-disassociation", Failed_disassociation
  ; "disassociated", Disassociated
  ]

let t_to_str =
  [ Associated, "associated"
  ; Pending_association, "pending-association"
  ; Pending_disassociation, "pending-disassociation"
  ; Failed_association, "failed-association"
  ; Failed_disassociation, "failed-disassociation"
  ; Disassociated, "disassociated"
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
