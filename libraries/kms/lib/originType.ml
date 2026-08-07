open Aws.BaseTypes

type t =
  | AWS_KMS
  | EXTERNAL
  | AWS_CLOUDHSM
  | EXTERNAL_KEY_STORE

let str_to_t =
  [ "EXTERNAL_KEY_STORE", EXTERNAL_KEY_STORE
  ; "AWS_CLOUDHSM", AWS_CLOUDHSM
  ; "EXTERNAL", EXTERNAL
  ; "AWS_KMS", AWS_KMS
  ]

let t_to_str =
  [ EXTERNAL_KEY_STORE, "EXTERNAL_KEY_STORE"
  ; AWS_CLOUDHSM, "AWS_CLOUDHSM"
  ; EXTERNAL, "EXTERNAL"
  ; AWS_KMS, "AWS_KMS"
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
