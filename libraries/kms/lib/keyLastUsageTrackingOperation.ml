open Aws.BaseTypes

type t =
  | Decrypt
  | DeriveSharedSecret
  | Encrypt
  | GenerateDataKey
  | GenerateDataKeyPair
  | GenerateDataKeyPairWithoutPlaintext
  | GenerateDataKeyWithoutPlaintext
  | GenerateMac
  | ReEncrypt
  | Sign
  | Verify
  | VerifyMac

let str_to_t =
  [ "VerifyMac", VerifyMac
  ; "Verify", Verify
  ; "Sign", Sign
  ; "ReEncrypt", ReEncrypt
  ; "GenerateMac", GenerateMac
  ; "GenerateDataKeyWithoutPlaintext", GenerateDataKeyWithoutPlaintext
  ; "GenerateDataKeyPairWithoutPlaintext", GenerateDataKeyPairWithoutPlaintext
  ; "GenerateDataKeyPair", GenerateDataKeyPair
  ; "GenerateDataKey", GenerateDataKey
  ; "Encrypt", Encrypt
  ; "DeriveSharedSecret", DeriveSharedSecret
  ; "Decrypt", Decrypt
  ]

let t_to_str =
  [ VerifyMac, "VerifyMac"
  ; Verify, "Verify"
  ; Sign, "Sign"
  ; ReEncrypt, "ReEncrypt"
  ; GenerateMac, "GenerateMac"
  ; GenerateDataKeyWithoutPlaintext, "GenerateDataKeyWithoutPlaintext"
  ; GenerateDataKeyPairWithoutPlaintext, "GenerateDataKeyPairWithoutPlaintext"
  ; GenerateDataKeyPair, "GenerateDataKeyPair"
  ; GenerateDataKey, "GenerateDataKey"
  ; Encrypt, "Encrypt"
  ; DeriveSharedSecret, "DeriveSharedSecret"
  ; Decrypt, "Decrypt"
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
