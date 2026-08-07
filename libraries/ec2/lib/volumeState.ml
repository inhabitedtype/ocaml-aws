open Aws.BaseTypes

type t =
  | Creating
  | Available
  | In_use
  | Deleting
  | Deleted
  | Error

let str_to_t =
  [ "error", Error
  ; "deleted", Deleted
  ; "deleting", Deleting
  ; "in-use", In_use
  ; "available", Available
  ; "creating", Creating
  ]

let t_to_str =
  [ Error, "error"
  ; Deleted, "deleted"
  ; Deleting, "deleting"
  ; In_use, "in-use"
  ; Available, "available"
  ; Creating, "creating"
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
