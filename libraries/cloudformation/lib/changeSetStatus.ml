open Aws.BaseTypes

type t =
  | CREATE_PENDING
  | CREATE_IN_PROGRESS
  | CREATE_COMPLETE
  | DELETE_PENDING
  | DELETE_IN_PROGRESS
  | DELETE_COMPLETE
  | DELETE_FAILED
  | FAILED

let str_to_t =
  [ "FAILED", FAILED
  ; "DELETE_FAILED", DELETE_FAILED
  ; "DELETE_COMPLETE", DELETE_COMPLETE
  ; "DELETE_IN_PROGRESS", DELETE_IN_PROGRESS
  ; "DELETE_PENDING", DELETE_PENDING
  ; "CREATE_COMPLETE", CREATE_COMPLETE
  ; "CREATE_IN_PROGRESS", CREATE_IN_PROGRESS
  ; "CREATE_PENDING", CREATE_PENDING
  ]

let t_to_str =
  [ FAILED, "FAILED"
  ; DELETE_FAILED, "DELETE_FAILED"
  ; DELETE_COMPLETE, "DELETE_COMPLETE"
  ; DELETE_IN_PROGRESS, "DELETE_IN_PROGRESS"
  ; DELETE_PENDING, "DELETE_PENDING"
  ; CREATE_COMPLETE, "CREATE_COMPLETE"
  ; CREATE_IN_PROGRESS, "CREATE_IN_PROGRESS"
  ; CREATE_PENDING, "CREATE_PENDING"
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
