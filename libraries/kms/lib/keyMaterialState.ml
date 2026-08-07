open Aws.BaseTypes

type t =
  | NON_CURRENT
  | CURRENT
  | PENDING_ROTATION
  | PENDING_MULTI_REGION_IMPORT_AND_ROTATION

let str_to_t =
  [ "PENDING_MULTI_REGION_IMPORT_AND_ROTATION", PENDING_MULTI_REGION_IMPORT_AND_ROTATION
  ; "PENDING_ROTATION", PENDING_ROTATION
  ; "CURRENT", CURRENT
  ; "NON_CURRENT", NON_CURRENT
  ]

let t_to_str =
  [ PENDING_MULTI_REGION_IMPORT_AND_ROTATION, "PENDING_MULTI_REGION_IMPORT_AND_ROTATION"
  ; PENDING_ROTATION, "PENDING_ROTATION"
  ; CURRENT, "CURRENT"
  ; NON_CURRENT, "NON_CURRENT"
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
