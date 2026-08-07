open Aws.BaseTypes

type t =
  | RSAES_PKCS1_V1_5
  | RSAES_OAEP_SHA_1
  | RSAES_OAEP_SHA_256
  | RSA_AES_KEY_WRAP_SHA_1
  | RSA_AES_KEY_WRAP_SHA_256
  | SM2PKE

let str_to_t =
  [ "SM2PKE", SM2PKE
  ; "RSA_AES_KEY_WRAP_SHA_256", RSA_AES_KEY_WRAP_SHA_256
  ; "RSA_AES_KEY_WRAP_SHA_1", RSA_AES_KEY_WRAP_SHA_1
  ; "RSAES_OAEP_SHA_256", RSAES_OAEP_SHA_256
  ; "RSAES_OAEP_SHA_1", RSAES_OAEP_SHA_1
  ; "RSAES_PKCS1_V1_5", RSAES_PKCS1_V1_5
  ]

let t_to_str =
  [ SM2PKE, "SM2PKE"
  ; RSA_AES_KEY_WRAP_SHA_256, "RSA_AES_KEY_WRAP_SHA_256"
  ; RSA_AES_KEY_WRAP_SHA_1, "RSA_AES_KEY_WRAP_SHA_1"
  ; RSAES_OAEP_SHA_256, "RSAES_OAEP_SHA_256"
  ; RSAES_OAEP_SHA_1, "RSAES_OAEP_SHA_1"
  ; RSAES_PKCS1_V1_5, "RSAES_PKCS1_V1_5"
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
