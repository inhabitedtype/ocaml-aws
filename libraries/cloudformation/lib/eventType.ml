open Aws.BaseTypes

type t =
  | STACK_EVENT
  | PROGRESS_EVENT
  | VALIDATION_ERROR
  | PROVISIONING_ERROR
  | HOOK_INVOCATION_ERROR

let str_to_t =
  [ "HOOK_INVOCATION_ERROR", HOOK_INVOCATION_ERROR
  ; "PROVISIONING_ERROR", PROVISIONING_ERROR
  ; "VALIDATION_ERROR", VALIDATION_ERROR
  ; "PROGRESS_EVENT", PROGRESS_EVENT
  ; "STACK_EVENT", STACK_EVENT
  ]

let t_to_str =
  [ HOOK_INVOCATION_ERROR, "HOOK_INVOCATION_ERROR"
  ; PROVISIONING_ERROR, "PROVISIONING_ERROR"
  ; VALIDATION_ERROR, "VALIDATION_ERROR"
  ; PROGRESS_EVENT, "PROGRESS_EVENT"
  ; STACK_EVENT, "STACK_EVENT"
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
