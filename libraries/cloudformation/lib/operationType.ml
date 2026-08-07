open Aws.BaseTypes

type t =
  | CREATE_STACK
  | UPDATE_STACK
  | DELETE_STACK
  | CONTINUE_ROLLBACK
  | ROLLBACK
  | CREATE_CHANGESET

let str_to_t =
  [ "CREATE_CHANGESET", CREATE_CHANGESET
  ; "ROLLBACK", ROLLBACK
  ; "CONTINUE_ROLLBACK", CONTINUE_ROLLBACK
  ; "DELETE_STACK", DELETE_STACK
  ; "UPDATE_STACK", UPDATE_STACK
  ; "CREATE_STACK", CREATE_STACK
  ]

let t_to_str =
  [ CREATE_CHANGESET, "CREATE_CHANGESET"
  ; ROLLBACK, "ROLLBACK"
  ; CONTINUE_ROLLBACK, "CONTINUE_ROLLBACK"
  ; DELETE_STACK, "DELETE_STACK"
  ; UPDATE_STACK, "UPDATE_STACK"
  ; CREATE_STACK, "CREATE_STACK"
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
