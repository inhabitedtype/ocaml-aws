open Aws.BaseTypes

type t =
  | NEW_KEY_MATERIAL
  | EXISTING_KEY_MATERIAL

let str_to_t =
  [ "EXISTING_KEY_MATERIAL", EXISTING_KEY_MATERIAL; "NEW_KEY_MATERIAL", NEW_KEY_MATERIAL ]

let t_to_str =
  [ EXISTING_KEY_MATERIAL, "EXISTING_KEY_MATERIAL"; NEW_KEY_MATERIAL, "NEW_KEY_MATERIAL" ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
