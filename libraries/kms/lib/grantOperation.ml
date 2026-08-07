open Aws.BaseTypes

type t =
  | Decrypt
  | Encrypt
  | GenerateDataKey
  | GenerateDataKeyWithoutPlaintext
  | ReEncryptFrom
  | ReEncryptTo
  | Sign
  | Verify
  | GetPublicKey
  | CreateGrant
  | RetireGrant
  | DescribeKey
  | GenerateDataKeyPair
  | GenerateDataKeyPairWithoutPlaintext
  | GenerateMac
  | VerifyMac
  | DeriveSharedSecret

let str_to_t =
  [ "DeriveSharedSecret", DeriveSharedSecret
  ; "VerifyMac", VerifyMac
  ; "GenerateMac", GenerateMac
  ; "GenerateDataKeyPairWithoutPlaintext", GenerateDataKeyPairWithoutPlaintext
  ; "GenerateDataKeyPair", GenerateDataKeyPair
  ; "DescribeKey", DescribeKey
  ; "RetireGrant", RetireGrant
  ; "CreateGrant", CreateGrant
  ; "GetPublicKey", GetPublicKey
  ; "Verify", Verify
  ; "Sign", Sign
  ; "ReEncryptTo", ReEncryptTo
  ; "ReEncryptFrom", ReEncryptFrom
  ; "GenerateDataKeyWithoutPlaintext", GenerateDataKeyWithoutPlaintext
  ; "GenerateDataKey", GenerateDataKey
  ; "Encrypt", Encrypt
  ; "Decrypt", Decrypt
  ]

let t_to_str =
  [ DeriveSharedSecret, "DeriveSharedSecret"
  ; VerifyMac, "VerifyMac"
  ; GenerateMac, "GenerateMac"
  ; GenerateDataKeyPairWithoutPlaintext, "GenerateDataKeyPairWithoutPlaintext"
  ; GenerateDataKeyPair, "GenerateDataKeyPair"
  ; DescribeKey, "DescribeKey"
  ; RetireGrant, "RetireGrant"
  ; CreateGrant, "CreateGrant"
  ; GetPublicKey, "GetPublicKey"
  ; Verify, "Verify"
  ; Sign, "Sign"
  ; ReEncryptTo, "ReEncryptTo"
  ; ReEncryptFrom, "ReEncryptFrom"
  ; GenerateDataKeyWithoutPlaintext, "GenerateDataKeyWithoutPlaintext"
  ; GenerateDataKey, "GenerateDataKey"
  ; Encrypt, "Encrypt"
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
