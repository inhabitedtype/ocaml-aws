open Aws.BaseTypes

type t =
  | RSASSA_PSS_SHA_256
  | RSASSA_PSS_SHA_384
  | RSASSA_PSS_SHA_512
  | RSASSA_PKCS1_V1_5_SHA_256
  | RSASSA_PKCS1_V1_5_SHA_384
  | RSASSA_PKCS1_V1_5_SHA_512
  | ECDSA_SHA_256
  | ECDSA_SHA_384
  | ECDSA_SHA_512
  | SM2DSA
  | ML_DSA_SHAKE_256
  | ED25519_SHA_512
  | ED25519_PH_SHA_512

let str_to_t =
  [ "ED25519_PH_SHA_512", ED25519_PH_SHA_512
  ; "ED25519_SHA_512", ED25519_SHA_512
  ; "ML_DSA_SHAKE_256", ML_DSA_SHAKE_256
  ; "SM2DSA", SM2DSA
  ; "ECDSA_SHA_512", ECDSA_SHA_512
  ; "ECDSA_SHA_384", ECDSA_SHA_384
  ; "ECDSA_SHA_256", ECDSA_SHA_256
  ; "RSASSA_PKCS1_V1_5_SHA_512", RSASSA_PKCS1_V1_5_SHA_512
  ; "RSASSA_PKCS1_V1_5_SHA_384", RSASSA_PKCS1_V1_5_SHA_384
  ; "RSASSA_PKCS1_V1_5_SHA_256", RSASSA_PKCS1_V1_5_SHA_256
  ; "RSASSA_PSS_SHA_512", RSASSA_PSS_SHA_512
  ; "RSASSA_PSS_SHA_384", RSASSA_PSS_SHA_384
  ; "RSASSA_PSS_SHA_256", RSASSA_PSS_SHA_256
  ]

let t_to_str =
  [ ED25519_PH_SHA_512, "ED25519_PH_SHA_512"
  ; ED25519_SHA_512, "ED25519_SHA_512"
  ; ML_DSA_SHAKE_256, "ML_DSA_SHAKE_256"
  ; SM2DSA, "SM2DSA"
  ; ECDSA_SHA_512, "ECDSA_SHA_512"
  ; ECDSA_SHA_384, "ECDSA_SHA_384"
  ; ECDSA_SHA_256, "ECDSA_SHA_256"
  ; RSASSA_PKCS1_V1_5_SHA_512, "RSASSA_PKCS1_V1_5_SHA_512"
  ; RSASSA_PKCS1_V1_5_SHA_384, "RSASSA_PKCS1_V1_5_SHA_384"
  ; RSASSA_PKCS1_V1_5_SHA_256, "RSASSA_PKCS1_V1_5_SHA_256"
  ; RSASSA_PSS_SHA_512, "RSASSA_PSS_SHA_512"
  ; RSASSA_PSS_SHA_384, "RSASSA_PSS_SHA_384"
  ; RSASSA_PSS_SHA_256, "RSASSA_PSS_SHA_256"
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
