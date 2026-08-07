open Aws.BaseTypes

type t =
  | Pending
  | Waiting_for_shutdown
  | Bundling
  | Storing
  | Cancelling
  | Complete
  | Failed

let str_to_t =
  [ "failed", Failed
  ; "complete", Complete
  ; "cancelling", Cancelling
  ; "storing", Storing
  ; "bundling", Bundling
  ; "waiting-for-shutdown", Waiting_for_shutdown
  ; "pending", Pending
  ]

let t_to_str =
  [ Failed, "failed"
  ; Complete, "complete"
  ; Cancelling, "cancelling"
  ; Storing, "storing"
  ; Bundling, "bundling"
  ; Waiting_for_shutdown, "waiting-for-shutdown"
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
