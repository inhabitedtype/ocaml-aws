open Aws.BaseTypes

type t =
  | InvalidInstanceID_Malformed
  | InvalidInstanceID_NotFound
  | IncorrectInstanceState
  | InstanceCreditSpecification_NotSupported

let str_to_t =
  [ "InstanceCreditSpecification.NotSupported", InstanceCreditSpecification_NotSupported
  ; "IncorrectInstanceState", IncorrectInstanceState
  ; "InvalidInstanceID.NotFound", InvalidInstanceID_NotFound
  ; "InvalidInstanceID.Malformed", InvalidInstanceID_Malformed
  ]

let t_to_str =
  [ InstanceCreditSpecification_NotSupported, "InstanceCreditSpecification.NotSupported"
  ; IncorrectInstanceState, "IncorrectInstanceState"
  ; InvalidInstanceID_NotFound, "InvalidInstanceID.NotFound"
  ; InvalidInstanceID_Malformed, "InvalidInstanceID.Malformed"
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
