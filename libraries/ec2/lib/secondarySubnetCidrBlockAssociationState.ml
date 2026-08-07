open Aws.BaseTypes

type t =
  | Associating
  | Associated
  | Association_failed
  | Disassociating
  | Disassociated
  | Disassociation_failed

let str_to_t =
  [ "disassociation-failed", Disassociation_failed
  ; "disassociated", Disassociated
  ; "disassociating", Disassociating
  ; "association-failed", Association_failed
  ; "associated", Associated
  ; "associating", Associating
  ]

let t_to_str =
  [ Disassociation_failed, "disassociation-failed"
  ; Disassociated, "disassociated"
  ; Disassociating, "disassociating"
  ; Association_failed, "association-failed"
  ; Associated, "associated"
  ; Associating, "associating"
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
