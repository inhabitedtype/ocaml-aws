open Aws.BaseTypes

type t =
  | Reserved_instances_id_invalid
  | Reserved_instances_not_in_queued_state
  | Unexpected_error

let str_to_t =
  [ "unexpected-error", Unexpected_error
  ; "reserved-instances-not-in-queued-state", Reserved_instances_not_in_queued_state
  ; "reserved-instances-id-invalid", Reserved_instances_id_invalid
  ]

let t_to_str =
  [ Unexpected_error, "unexpected-error"
  ; Reserved_instances_not_in_queued_state, "reserved-instances-not-in-queued-state"
  ; Reserved_instances_id_invalid, "reserved-instances-id-invalid"
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
