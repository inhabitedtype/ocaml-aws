open Aws.BaseTypes

type t =
  | CREATE_PENDING
  | UPDATE_PENDING
  | DELETE_PENDING
  | CREATE_IN_PROGRESS
  | UPDATE_IN_PROGRESS
  | DELETE_IN_PROGRESS
  | FAILED
  | COMPLETE

let str_to_t =
  [ "COMPLETE", COMPLETE
  ; "FAILED", FAILED
  ; "DELETE_IN_PROGRESS", DELETE_IN_PROGRESS
  ; "UPDATE_IN_PROGRESS", UPDATE_IN_PROGRESS
  ; "CREATE_IN_PROGRESS", CREATE_IN_PROGRESS
  ; "DELETE_PENDING", DELETE_PENDING
  ; "UPDATE_PENDING", UPDATE_PENDING
  ; "CREATE_PENDING", CREATE_PENDING
  ]

let t_to_str =
  [ COMPLETE, "COMPLETE"
  ; FAILED, "FAILED"
  ; DELETE_IN_PROGRESS, "DELETE_IN_PROGRESS"
  ; UPDATE_IN_PROGRESS, "UPDATE_IN_PROGRESS"
  ; CREATE_IN_PROGRESS, "CREATE_IN_PROGRESS"
  ; DELETE_PENDING, "DELETE_PENDING"
  ; UPDATE_PENDING, "UPDATE_PENDING"
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
