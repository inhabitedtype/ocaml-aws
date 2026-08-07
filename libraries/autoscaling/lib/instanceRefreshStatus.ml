open Aws.BaseTypes

type t =
  | Pending
  | InProgress
  | Successful
  | Failed
  | Cancelling
  | Cancelled
  | RollbackInProgress
  | RollbackFailed
  | RollbackSuccessful
  | Baking

let str_to_t =
  [ "Baking", Baking
  ; "RollbackSuccessful", RollbackSuccessful
  ; "RollbackFailed", RollbackFailed
  ; "RollbackInProgress", RollbackInProgress
  ; "Cancelled", Cancelled
  ; "Cancelling", Cancelling
  ; "Failed", Failed
  ; "Successful", Successful
  ; "InProgress", InProgress
  ; "Pending", Pending
  ]

let t_to_str =
  [ Baking, "Baking"
  ; RollbackSuccessful, "RollbackSuccessful"
  ; RollbackFailed, "RollbackFailed"
  ; RollbackInProgress, "RollbackInProgress"
  ; Cancelled, "Cancelled"
  ; Cancelling, "Cancelling"
  ; Failed, "Failed"
  ; Successful, "Successful"
  ; InProgress, "InProgress"
  ; Pending, "Pending"
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
