open Aws.BaseTypes

type t =
  | COMPLETED
  | FAILED
  | PARTIAL_SUCCESS
  | IN_PROGRESS
  | STOPPED

let str_to_t =
  [ "STOPPED", STOPPED
  ; "IN_PROGRESS", IN_PROGRESS
  ; "PARTIAL_SUCCESS", PARTIAL_SUCCESS
  ; "FAILED", FAILED
  ; "COMPLETED", COMPLETED
  ]

let t_to_str =
  [ STOPPED, "STOPPED"
  ; IN_PROGRESS, "IN_PROGRESS"
  ; PARTIAL_SUCCESS, "PARTIAL_SUCCESS"
  ; FAILED, "FAILED"
  ; COMPLETED, "COMPLETED"
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
