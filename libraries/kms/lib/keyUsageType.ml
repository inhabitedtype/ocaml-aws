open Aws.BaseTypes

type t =
  | SIGN_VERIFY
  | ENCRYPT_DECRYPT
  | GENERATE_VERIFY_MAC
  | KEY_AGREEMENT

let str_to_t =
  [ "KEY_AGREEMENT", KEY_AGREEMENT
  ; "GENERATE_VERIFY_MAC", GENERATE_VERIFY_MAC
  ; "ENCRYPT_DECRYPT", ENCRYPT_DECRYPT
  ; "SIGN_VERIFY", SIGN_VERIFY
  ]

let t_to_str =
  [ KEY_AGREEMENT, "KEY_AGREEMENT"
  ; GENERATE_VERIFY_MAC, "GENERATE_VERIFY_MAC"
  ; ENCRYPT_DECRYPT, "ENCRYPT_DECRYPT"
  ; SIGN_VERIFY, "SIGN_VERIFY"
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
