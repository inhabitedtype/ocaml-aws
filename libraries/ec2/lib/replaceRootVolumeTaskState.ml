open Aws.BaseTypes

type t =
  | Pending
  | In_progress
  | Failing
  | Succeeded
  | Failed
  | Failed_detached

let str_to_t =
  [ "failed-detached", Failed_detached
  ; "failed", Failed
  ; "succeeded", Succeeded
  ; "failing", Failing
  ; "in-progress", In_progress
  ; "pending", Pending
  ]

let t_to_str =
  [ Failed_detached, "failed-detached"
  ; Failed, "failed"
  ; Succeeded, "succeeded"
  ; Failing, "failing"
  ; In_progress, "in-progress"
  ; Pending, "pending"
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
