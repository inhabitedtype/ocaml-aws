open Aws.BaseTypes

type t =
  | MUTUALLY_EXCLUSIVE_PROPERTIES
  | UNSUPPORTED_PROPERTIES
  | MUTUALLY_EXCLUSIVE_TYPES
  | EXCLUDED_PROPERTIES
  | EXCLUDED_RESOURCES

let str_to_t =
  [ "EXCLUDED_RESOURCES", EXCLUDED_RESOURCES
  ; "EXCLUDED_PROPERTIES", EXCLUDED_PROPERTIES
  ; "MUTUALLY_EXCLUSIVE_TYPES", MUTUALLY_EXCLUSIVE_TYPES
  ; "UNSUPPORTED_PROPERTIES", UNSUPPORTED_PROPERTIES
  ; "MUTUALLY_EXCLUSIVE_PROPERTIES", MUTUALLY_EXCLUSIVE_PROPERTIES
  ]

let t_to_str =
  [ EXCLUDED_RESOURCES, "EXCLUDED_RESOURCES"
  ; EXCLUDED_PROPERTIES, "EXCLUDED_PROPERTIES"
  ; MUTUALLY_EXCLUSIVE_TYPES, "MUTUALLY_EXCLUSIVE_TYPES"
  ; UNSUPPORTED_PROPERTIES, "UNSUPPORTED_PROPERTIES"
  ; MUTUALLY_EXCLUSIVE_PROPERTIES, "MUTUALLY_EXCLUSIVE_PROPERTIES"
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
