open Aws
open Aws.BaseTypes
open CalendarLib

type calendar = Calendar.t

module MultiRegionKey = struct
  type t =
    { arn : String.t option
    ; region : String.t option
    }

  let make ?arn ?region () = { arn; region }

  let parse xml =
    Some
      { arn = Util.option_bind (Xml.member "Arn" xml) String.parse
      ; region = Util.option_bind (Xml.member "Region" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.region (fun f -> Query.Pair ("Region", String.to_query f))
         ; Util.option_map v.arn (fun f -> Query.Pair ("Arn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.region (fun f -> "region", String.to_json f)
         ; Util.option_map v.arn (fun f -> "arn", String.to_json f)
         ])

  let of_json j =
    { arn = Util.option_map (Json.lookup j "arn") String.of_json
    ; region = Util.option_map (Json.lookup j "region") String.of_json
    }
end

module EncryptionContextType = struct
  type t = (String.t, String.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v = Query.to_query_hashtbl String.to_string String.to_query v

  let to_json v =
    `Assoc
      (Hashtbl.fold (fun k v acc -> (String.to_string k, String.to_json v) :: acc) v [])

  let of_json j = Json.to_hashtbl String.of_string String.of_json j
end

module GrantOperation = struct
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

  let str_to_t =
    [ "GenerateDataKeyPairWithoutPlaintext", GenerateDataKeyPairWithoutPlaintext
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
    [ GenerateDataKeyPairWithoutPlaintext, "GenerateDataKeyPairWithoutPlaintext"
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

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module EncryptionAlgorithmSpec = struct
  type t =
    | SYMMETRIC_DEFAULT
    | RSAES_OAEP_SHA_1
    | RSAES_OAEP_SHA_256

  let str_to_t =
    [ "RSAES_OAEP_SHA_256", RSAES_OAEP_SHA_256
    ; "RSAES_OAEP_SHA_1", RSAES_OAEP_SHA_1
    ; "SYMMETRIC_DEFAULT", SYMMETRIC_DEFAULT
    ]

  let t_to_str =
    [ RSAES_OAEP_SHA_256, "RSAES_OAEP_SHA_256"
    ; RSAES_OAEP_SHA_1, "RSAES_OAEP_SHA_1"
    ; SYMMETRIC_DEFAULT, "SYMMETRIC_DEFAULT"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module MultiRegionKeyList = struct
  type t = MultiRegionKey.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map MultiRegionKey.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list MultiRegionKey.to_query v

  let to_json v = `List (List.map MultiRegionKey.to_json v)

  let of_json j = Json.to_list MultiRegionKey.of_json j
end

module MultiRegionKeyType = struct
  type t =
    | PRIMARY
    | REPLICA

  let str_to_t = [ "REPLICA", REPLICA; "PRIMARY", PRIMARY ]

  let t_to_str = [ REPLICA, "REPLICA"; PRIMARY, "PRIMARY" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module SigningAlgorithmSpec = struct
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

  let str_to_t =
    [ "ECDSA_SHA_512", ECDSA_SHA_512
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
    [ ECDSA_SHA_512, "ECDSA_SHA_512"
    ; ECDSA_SHA_384, "ECDSA_SHA_384"
    ; ECDSA_SHA_256, "ECDSA_SHA_256"
    ; RSASSA_PKCS1_V1_5_SHA_512, "RSASSA_PKCS1_V1_5_SHA_512"
    ; RSASSA_PKCS1_V1_5_SHA_384, "RSASSA_PKCS1_V1_5_SHA_384"
    ; RSASSA_PKCS1_V1_5_SHA_256, "RSASSA_PKCS1_V1_5_SHA_256"
    ; RSASSA_PSS_SHA_512, "RSASSA_PSS_SHA_512"
    ; RSASSA_PSS_SHA_384, "RSASSA_PSS_SHA_384"
    ; RSASSA_PSS_SHA_256, "RSASSA_PSS_SHA_256"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ConnectionErrorCodeType = struct
  type t =
    | INVALID_CREDENTIALS
    | CLUSTER_NOT_FOUND
    | NETWORK_ERRORS
    | INTERNAL_ERROR
    | INSUFFICIENT_CLOUDHSM_HSMS
    | USER_LOCKED_OUT
    | USER_NOT_FOUND
    | USER_LOGGED_IN
    | SUBNET_NOT_FOUND

  let str_to_t =
    [ "SUBNET_NOT_FOUND", SUBNET_NOT_FOUND
    ; "USER_LOGGED_IN", USER_LOGGED_IN
    ; "USER_NOT_FOUND", USER_NOT_FOUND
    ; "USER_LOCKED_OUT", USER_LOCKED_OUT
    ; "INSUFFICIENT_CLOUDHSM_HSMS", INSUFFICIENT_CLOUDHSM_HSMS
    ; "INTERNAL_ERROR", INTERNAL_ERROR
    ; "NETWORK_ERRORS", NETWORK_ERRORS
    ; "CLUSTER_NOT_FOUND", CLUSTER_NOT_FOUND
    ; "INVALID_CREDENTIALS", INVALID_CREDENTIALS
    ]

  let t_to_str =
    [ SUBNET_NOT_FOUND, "SUBNET_NOT_FOUND"
    ; USER_LOGGED_IN, "USER_LOGGED_IN"
    ; USER_NOT_FOUND, "USER_NOT_FOUND"
    ; USER_LOCKED_OUT, "USER_LOCKED_OUT"
    ; INSUFFICIENT_CLOUDHSM_HSMS, "INSUFFICIENT_CLOUDHSM_HSMS"
    ; INTERNAL_ERROR, "INTERNAL_ERROR"
    ; NETWORK_ERRORS, "NETWORK_ERRORS"
    ; CLUSTER_NOT_FOUND, "CLUSTER_NOT_FOUND"
    ; INVALID_CREDENTIALS, "INVALID_CREDENTIALS"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ConnectionStateType = struct
  type t =
    | CONNECTED
    | CONNECTING
    | FAILED
    | DISCONNECTED
    | DISCONNECTING

  let str_to_t =
    [ "DISCONNECTING", DISCONNECTING
    ; "DISCONNECTED", DISCONNECTED
    ; "FAILED", FAILED
    ; "CONNECTING", CONNECTING
    ; "CONNECTED", CONNECTED
    ]

  let t_to_str =
    [ DISCONNECTING, "DISCONNECTING"
    ; DISCONNECTED, "DISCONNECTED"
    ; FAILED, "FAILED"
    ; CONNECTING, "CONNECTING"
    ; CONNECTED, "CONNECTED"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module GrantConstraints = struct
  type t =
    { encryption_context_subset : EncryptionContextType.t option
    ; encryption_context_equals : EncryptionContextType.t option
    }

  let make ?encryption_context_subset ?encryption_context_equals () =
    { encryption_context_subset; encryption_context_equals }

  let parse xml =
    Some
      { encryption_context_subset =
          Util.option_bind
            (Xml.member "EncryptionContextSubset" xml)
            EncryptionContextType.parse
      ; encryption_context_equals =
          Util.option_bind
            (Xml.member "EncryptionContextEquals" xml)
            EncryptionContextType.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.encryption_context_equals (fun f ->
               Query.Pair ("EncryptionContextEquals", EncryptionContextType.to_query f))
         ; Util.option_map v.encryption_context_subset (fun f ->
               Query.Pair ("EncryptionContextSubset", EncryptionContextType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.encryption_context_equals (fun f ->
               "encryption_context_equals", EncryptionContextType.to_json f)
         ; Util.option_map v.encryption_context_subset (fun f ->
               "encryption_context_subset", EncryptionContextType.to_json f)
         ])

  let of_json j =
    { encryption_context_subset =
        Util.option_map
          (Json.lookup j "encryption_context_subset")
          EncryptionContextType.of_json
    ; encryption_context_equals =
        Util.option_map
          (Json.lookup j "encryption_context_equals")
          EncryptionContextType.of_json
    }
end

module GrantOperationList = struct
  type t = GrantOperation.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map GrantOperation.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list GrantOperation.to_query v

  let to_json v = `List (List.map GrantOperation.to_json v)

  let of_json j = Json.to_list GrantOperation.of_json j
end

module Tag = struct
  type t =
    { tag_key : String.t
    ; tag_value : String.t
    }

  let make ~tag_key ~tag_value () = { tag_key; tag_value }

  let parse xml =
    Some
      { tag_key =
          Xml.required "TagKey" (Util.option_bind (Xml.member "TagKey" xml) String.parse)
      ; tag_value =
          Xml.required
            "TagValue"
            (Util.option_bind (Xml.member "TagValue" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TagValue", String.to_query v.tag_value))
         ; Some (Query.Pair ("TagKey", String.to_query v.tag_key))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tag_value", String.to_json v.tag_value)
         ; Some ("tag_key", String.to_json v.tag_key)
         ])

  let of_json j =
    { tag_key = String.of_json (Util.of_option_exn (Json.lookup j "tag_key"))
    ; tag_value = String.of_json (Util.of_option_exn (Json.lookup j "tag_value"))
    }
end

module KeyListEntry = struct
  type t =
    { key_id : String.t option
    ; key_arn : String.t option
    }

  let make ?key_id ?key_arn () = { key_id; key_arn }

  let parse xml =
    Some
      { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; key_arn = Util.option_bind (Xml.member "KeyArn" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.key_arn (fun f -> Query.Pair ("KeyArn", String.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.key_arn (fun f -> "key_arn", String.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ])

  let of_json j =
    { key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; key_arn = Util.option_map (Json.lookup j "key_arn") String.of_json
    }
end

module CustomerMasterKeySpec = struct
  type t =
    | RSA_2048
    | RSA_3072
    | RSA_4096
    | ECC_NIST_P256
    | ECC_NIST_P384
    | ECC_NIST_P521
    | ECC_SECG_P256K1
    | SYMMETRIC_DEFAULT

  let str_to_t =
    [ "SYMMETRIC_DEFAULT", SYMMETRIC_DEFAULT
    ; "ECC_SECG_P256K1", ECC_SECG_P256K1
    ; "ECC_NIST_P521", ECC_NIST_P521
    ; "ECC_NIST_P384", ECC_NIST_P384
    ; "ECC_NIST_P256", ECC_NIST_P256
    ; "RSA_4096", RSA_4096
    ; "RSA_3072", RSA_3072
    ; "RSA_2048", RSA_2048
    ]

  let t_to_str =
    [ SYMMETRIC_DEFAULT, "SYMMETRIC_DEFAULT"
    ; ECC_SECG_P256K1, "ECC_SECG_P256K1"
    ; ECC_NIST_P521, "ECC_NIST_P521"
    ; ECC_NIST_P384, "ECC_NIST_P384"
    ; ECC_NIST_P256, "ECC_NIST_P256"
    ; RSA_4096, "RSA_4096"
    ; RSA_3072, "RSA_3072"
    ; RSA_2048, "RSA_2048"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module EncryptionAlgorithmSpecList = struct
  type t = EncryptionAlgorithmSpec.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map EncryptionAlgorithmSpec.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list EncryptionAlgorithmSpec.to_query v

  let to_json v = `List (List.map EncryptionAlgorithmSpec.to_json v)

  let of_json j = Json.to_list EncryptionAlgorithmSpec.of_json j
end

module ExpirationModelType = struct
  type t =
    | KEY_MATERIAL_EXPIRES
    | KEY_MATERIAL_DOES_NOT_EXPIRE

  let str_to_t =
    [ "KEY_MATERIAL_DOES_NOT_EXPIRE", KEY_MATERIAL_DOES_NOT_EXPIRE
    ; "KEY_MATERIAL_EXPIRES", KEY_MATERIAL_EXPIRES
    ]

  let t_to_str =
    [ KEY_MATERIAL_DOES_NOT_EXPIRE, "KEY_MATERIAL_DOES_NOT_EXPIRE"
    ; KEY_MATERIAL_EXPIRES, "KEY_MATERIAL_EXPIRES"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module KeyManagerType = struct
  type t =
    | AWS
    | CUSTOMER

  let str_to_t = [ "CUSTOMER", CUSTOMER; "AWS", AWS ]

  let t_to_str = [ CUSTOMER, "CUSTOMER"; AWS, "AWS" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module KeySpec = struct
  type t =
    | RSA_2048
    | RSA_3072
    | RSA_4096
    | ECC_NIST_P256
    | ECC_NIST_P384
    | ECC_NIST_P521
    | ECC_SECG_P256K1
    | SYMMETRIC_DEFAULT

  let str_to_t =
    [ "SYMMETRIC_DEFAULT", SYMMETRIC_DEFAULT
    ; "ECC_SECG_P256K1", ECC_SECG_P256K1
    ; "ECC_NIST_P521", ECC_NIST_P521
    ; "ECC_NIST_P384", ECC_NIST_P384
    ; "ECC_NIST_P256", ECC_NIST_P256
    ; "RSA_4096", RSA_4096
    ; "RSA_3072", RSA_3072
    ; "RSA_2048", RSA_2048
    ]

  let t_to_str =
    [ SYMMETRIC_DEFAULT, "SYMMETRIC_DEFAULT"
    ; ECC_SECG_P256K1, "ECC_SECG_P256K1"
    ; ECC_NIST_P521, "ECC_NIST_P521"
    ; ECC_NIST_P384, "ECC_NIST_P384"
    ; ECC_NIST_P256, "ECC_NIST_P256"
    ; RSA_4096, "RSA_4096"
    ; RSA_3072, "RSA_3072"
    ; RSA_2048, "RSA_2048"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module KeyState = struct
  type t =
    | Creating
    | Enabled
    | Disabled
    | PendingDeletion
    | PendingImport
    | PendingReplicaDeletion
    | Unavailable
    | Updating

  let str_to_t =
    [ "Updating", Updating
    ; "Unavailable", Unavailable
    ; "PendingReplicaDeletion", PendingReplicaDeletion
    ; "PendingImport", PendingImport
    ; "PendingDeletion", PendingDeletion
    ; "Disabled", Disabled
    ; "Enabled", Enabled
    ; "Creating", Creating
    ]

  let t_to_str =
    [ Updating, "Updating"
    ; Unavailable, "Unavailable"
    ; PendingReplicaDeletion, "PendingReplicaDeletion"
    ; PendingImport, "PendingImport"
    ; PendingDeletion, "PendingDeletion"
    ; Disabled, "Disabled"
    ; Enabled, "Enabled"
    ; Creating, "Creating"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module KeyUsageType = struct
  type t =
    | SIGN_VERIFY
    | ENCRYPT_DECRYPT

  let str_to_t = [ "ENCRYPT_DECRYPT", ENCRYPT_DECRYPT; "SIGN_VERIFY", SIGN_VERIFY ]

  let t_to_str = [ ENCRYPT_DECRYPT, "ENCRYPT_DECRYPT"; SIGN_VERIFY, "SIGN_VERIFY" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module MultiRegionConfiguration = struct
  type t =
    { multi_region_key_type : MultiRegionKeyType.t option
    ; primary_key : MultiRegionKey.t option
    ; replica_keys : MultiRegionKeyList.t
    }

  let make ?multi_region_key_type ?primary_key ?(replica_keys = []) () =
    { multi_region_key_type; primary_key; replica_keys }

  let parse xml =
    Some
      { multi_region_key_type =
          Util.option_bind (Xml.member "MultiRegionKeyType" xml) MultiRegionKeyType.parse
      ; primary_key = Util.option_bind (Xml.member "PrimaryKey" xml) MultiRegionKey.parse
      ; replica_keys =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ReplicaKeys" xml) MultiRegionKeyList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("ReplicaKeys.member", MultiRegionKeyList.to_query v.replica_keys))
         ; Util.option_map v.primary_key (fun f ->
               Query.Pair ("PrimaryKey", MultiRegionKey.to_query f))
         ; Util.option_map v.multi_region_key_type (fun f ->
               Query.Pair ("MultiRegionKeyType", MultiRegionKeyType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("replica_keys", MultiRegionKeyList.to_json v.replica_keys)
         ; Util.option_map v.primary_key (fun f ->
               "primary_key", MultiRegionKey.to_json f)
         ; Util.option_map v.multi_region_key_type (fun f ->
               "multi_region_key_type", MultiRegionKeyType.to_json f)
         ])

  let of_json j =
    { multi_region_key_type =
        Util.option_map (Json.lookup j "multi_region_key_type") MultiRegionKeyType.of_json
    ; primary_key = Util.option_map (Json.lookup j "primary_key") MultiRegionKey.of_json
    ; replica_keys =
        MultiRegionKeyList.of_json (Util.of_option_exn (Json.lookup j "replica_keys"))
    }
end

module OriginType = struct
  type t =
    | AWS_KMS
    | EXTERNAL
    | AWS_CLOUDHSM

  let str_to_t =
    [ "AWS_CLOUDHSM", AWS_CLOUDHSM; "EXTERNAL", EXTERNAL; "AWS_KMS", AWS_KMS ]

  let t_to_str =
    [ AWS_CLOUDHSM, "AWS_CLOUDHSM"; EXTERNAL, "EXTERNAL"; AWS_KMS, "AWS_KMS" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module SigningAlgorithmSpecList = struct
  type t = SigningAlgorithmSpec.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map SigningAlgorithmSpec.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list SigningAlgorithmSpec.to_query v

  let to_json v = `List (List.map SigningAlgorithmSpec.to_json v)

  let of_json j = Json.to_list SigningAlgorithmSpec.of_json j
end

module CustomKeyStoresListEntry = struct
  type t =
    { custom_key_store_id : String.t option
    ; custom_key_store_name : String.t option
    ; cloud_hsm_cluster_id : String.t option
    ; trust_anchor_certificate : String.t option
    ; connection_state : ConnectionStateType.t option
    ; connection_error_code : ConnectionErrorCodeType.t option
    ; creation_date : DateTime.t option
    }

  let make
      ?custom_key_store_id
      ?custom_key_store_name
      ?cloud_hsm_cluster_id
      ?trust_anchor_certificate
      ?connection_state
      ?connection_error_code
      ?creation_date
      () =
    { custom_key_store_id
    ; custom_key_store_name
    ; cloud_hsm_cluster_id
    ; trust_anchor_certificate
    ; connection_state
    ; connection_error_code
    ; creation_date
    }

  let parse xml =
    Some
      { custom_key_store_id =
          Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse
      ; custom_key_store_name =
          Util.option_bind (Xml.member "CustomKeyStoreName" xml) String.parse
      ; cloud_hsm_cluster_id =
          Util.option_bind (Xml.member "CloudHsmClusterId" xml) String.parse
      ; trust_anchor_certificate =
          Util.option_bind (Xml.member "TrustAnchorCertificate" xml) String.parse
      ; connection_state =
          Util.option_bind (Xml.member "ConnectionState" xml) ConnectionStateType.parse
      ; connection_error_code =
          Util.option_bind
            (Xml.member "ConnectionErrorCode" xml)
            ConnectionErrorCodeType.parse
      ; creation_date = Util.option_bind (Xml.member "CreationDate" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.creation_date (fun f ->
               Query.Pair ("CreationDate", DateTime.to_query f))
         ; Util.option_map v.connection_error_code (fun f ->
               Query.Pair ("ConnectionErrorCode", ConnectionErrorCodeType.to_query f))
         ; Util.option_map v.connection_state (fun f ->
               Query.Pair ("ConnectionState", ConnectionStateType.to_query f))
         ; Util.option_map v.trust_anchor_certificate (fun f ->
               Query.Pair ("TrustAnchorCertificate", String.to_query f))
         ; Util.option_map v.cloud_hsm_cluster_id (fun f ->
               Query.Pair ("CloudHsmClusterId", String.to_query f))
         ; Util.option_map v.custom_key_store_name (fun f ->
               Query.Pair ("CustomKeyStoreName", String.to_query f))
         ; Util.option_map v.custom_key_store_id (fun f ->
               Query.Pair ("CustomKeyStoreId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.creation_date (fun f -> "creation_date", DateTime.to_json f)
         ; Util.option_map v.connection_error_code (fun f ->
               "connection_error_code", ConnectionErrorCodeType.to_json f)
         ; Util.option_map v.connection_state (fun f ->
               "connection_state", ConnectionStateType.to_json f)
         ; Util.option_map v.trust_anchor_certificate (fun f ->
               "trust_anchor_certificate", String.to_json f)
         ; Util.option_map v.cloud_hsm_cluster_id (fun f ->
               "cloud_hsm_cluster_id", String.to_json f)
         ; Util.option_map v.custom_key_store_name (fun f ->
               "custom_key_store_name", String.to_json f)
         ; Util.option_map v.custom_key_store_id (fun f ->
               "custom_key_store_id", String.to_json f)
         ])

  let of_json j =
    { custom_key_store_id =
        Util.option_map (Json.lookup j "custom_key_store_id") String.of_json
    ; custom_key_store_name =
        Util.option_map (Json.lookup j "custom_key_store_name") String.of_json
    ; cloud_hsm_cluster_id =
        Util.option_map (Json.lookup j "cloud_hsm_cluster_id") String.of_json
    ; trust_anchor_certificate =
        Util.option_map (Json.lookup j "trust_anchor_certificate") String.of_json
    ; connection_state =
        Util.option_map (Json.lookup j "connection_state") ConnectionStateType.of_json
    ; connection_error_code =
        Util.option_map
          (Json.lookup j "connection_error_code")
          ConnectionErrorCodeType.of_json
    ; creation_date = Util.option_map (Json.lookup j "creation_date") DateTime.of_json
    }
end

module GrantListEntry = struct
  type t =
    { key_id : String.t option
    ; grant_id : String.t option
    ; name : String.t option
    ; creation_date : DateTime.t option
    ; grantee_principal : String.t option
    ; retiring_principal : String.t option
    ; issuing_account : String.t option
    ; operations : GrantOperationList.t
    ; constraints : GrantConstraints.t option
    }

  let make
      ?key_id
      ?grant_id
      ?name
      ?creation_date
      ?grantee_principal
      ?retiring_principal
      ?issuing_account
      ?(operations = [])
      ?constraints
      () =
    { key_id
    ; grant_id
    ; name
    ; creation_date
    ; grantee_principal
    ; retiring_principal
    ; issuing_account
    ; operations
    ; constraints
    }

  let parse xml =
    Some
      { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; grant_id = Util.option_bind (Xml.member "GrantId" xml) String.parse
      ; name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; creation_date = Util.option_bind (Xml.member "CreationDate" xml) DateTime.parse
      ; grantee_principal =
          Util.option_bind (Xml.member "GranteePrincipal" xml) String.parse
      ; retiring_principal =
          Util.option_bind (Xml.member "RetiringPrincipal" xml) String.parse
      ; issuing_account = Util.option_bind (Xml.member "IssuingAccount" xml) String.parse
      ; operations =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Operations" xml) GrantOperationList.parse)
      ; constraints =
          Util.option_bind (Xml.member "Constraints" xml) GrantConstraints.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.constraints (fun f ->
               Query.Pair ("Constraints", GrantConstraints.to_query f))
         ; Some
             (Query.Pair ("Operations.member", GrantOperationList.to_query v.operations))
         ; Util.option_map v.issuing_account (fun f ->
               Query.Pair ("IssuingAccount", String.to_query f))
         ; Util.option_map v.retiring_principal (fun f ->
               Query.Pair ("RetiringPrincipal", String.to_query f))
         ; Util.option_map v.grantee_principal (fun f ->
               Query.Pair ("GranteePrincipal", String.to_query f))
         ; Util.option_map v.creation_date (fun f ->
               Query.Pair ("CreationDate", DateTime.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ; Util.option_map v.grant_id (fun f -> Query.Pair ("GrantId", String.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.constraints (fun f ->
               "constraints", GrantConstraints.to_json f)
         ; Some ("operations", GrantOperationList.to_json v.operations)
         ; Util.option_map v.issuing_account (fun f ->
               "issuing_account", String.to_json f)
         ; Util.option_map v.retiring_principal (fun f ->
               "retiring_principal", String.to_json f)
         ; Util.option_map v.grantee_principal (fun f ->
               "grantee_principal", String.to_json f)
         ; Util.option_map v.creation_date (fun f -> "creation_date", DateTime.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ; Util.option_map v.grant_id (fun f -> "grant_id", String.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ])

  let of_json j =
    { key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; grant_id = Util.option_map (Json.lookup j "grant_id") String.of_json
    ; name = Util.option_map (Json.lookup j "name") String.of_json
    ; creation_date = Util.option_map (Json.lookup j "creation_date") DateTime.of_json
    ; grantee_principal =
        Util.option_map (Json.lookup j "grantee_principal") String.of_json
    ; retiring_principal =
        Util.option_map (Json.lookup j "retiring_principal") String.of_json
    ; issuing_account = Util.option_map (Json.lookup j "issuing_account") String.of_json
    ; operations =
        GrantOperationList.of_json (Util.of_option_exn (Json.lookup j "operations"))
    ; constraints = Util.option_map (Json.lookup j "constraints") GrantConstraints.of_json
    }
end

module AliasListEntry = struct
  type t =
    { alias_name : String.t option
    ; alias_arn : String.t option
    ; target_key_id : String.t option
    ; creation_date : DateTime.t option
    ; last_updated_date : DateTime.t option
    }

  let make ?alias_name ?alias_arn ?target_key_id ?creation_date ?last_updated_date () =
    { alias_name; alias_arn; target_key_id; creation_date; last_updated_date }

  let parse xml =
    Some
      { alias_name = Util.option_bind (Xml.member "AliasName" xml) String.parse
      ; alias_arn = Util.option_bind (Xml.member "AliasArn" xml) String.parse
      ; target_key_id = Util.option_bind (Xml.member "TargetKeyId" xml) String.parse
      ; creation_date = Util.option_bind (Xml.member "CreationDate" xml) DateTime.parse
      ; last_updated_date =
          Util.option_bind (Xml.member "LastUpdatedDate" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.last_updated_date (fun f ->
               Query.Pair ("LastUpdatedDate", DateTime.to_query f))
         ; Util.option_map v.creation_date (fun f ->
               Query.Pair ("CreationDate", DateTime.to_query f))
         ; Util.option_map v.target_key_id (fun f ->
               Query.Pair ("TargetKeyId", String.to_query f))
         ; Util.option_map v.alias_arn (fun f ->
               Query.Pair ("AliasArn", String.to_query f))
         ; Util.option_map v.alias_name (fun f ->
               Query.Pair ("AliasName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.last_updated_date (fun f ->
               "last_updated_date", DateTime.to_json f)
         ; Util.option_map v.creation_date (fun f -> "creation_date", DateTime.to_json f)
         ; Util.option_map v.target_key_id (fun f -> "target_key_id", String.to_json f)
         ; Util.option_map v.alias_arn (fun f -> "alias_arn", String.to_json f)
         ; Util.option_map v.alias_name (fun f -> "alias_name", String.to_json f)
         ])

  let of_json j =
    { alias_name = Util.option_map (Json.lookup j "alias_name") String.of_json
    ; alias_arn = Util.option_map (Json.lookup j "alias_arn") String.of_json
    ; target_key_id = Util.option_map (Json.lookup j "target_key_id") String.of_json
    ; creation_date = Util.option_map (Json.lookup j "creation_date") DateTime.of_json
    ; last_updated_date =
        Util.option_map (Json.lookup j "last_updated_date") DateTime.of_json
    }
end

module TagList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Tag.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Json.to_list Tag.of_json j
end

module AlgorithmSpec = struct
  type t =
    | RSAES_PKCS1_V1_5
    | RSAES_OAEP_SHA_1
    | RSAES_OAEP_SHA_256

  let str_to_t =
    [ "RSAES_OAEP_SHA_256", RSAES_OAEP_SHA_256
    ; "RSAES_OAEP_SHA_1", RSAES_OAEP_SHA_1
    ; "RSAES_PKCS1_V1_5", RSAES_PKCS1_V1_5
    ]

  let t_to_str =
    [ RSAES_OAEP_SHA_256, "RSAES_OAEP_SHA_256"
    ; RSAES_OAEP_SHA_1, "RSAES_OAEP_SHA_1"
    ; RSAES_PKCS1_V1_5, "RSAES_PKCS1_V1_5"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module WrappingKeySpec = struct
  type t = RSA_2048

  let str_to_t = [ "RSA_2048", RSA_2048 ]

  let t_to_str = [ RSA_2048, "RSA_2048" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module KeyList = struct
  type t = KeyListEntry.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map KeyListEntry.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list KeyListEntry.to_query v

  let to_json v = `List (List.map KeyListEntry.to_json v)

  let of_json j = Json.to_list KeyListEntry.of_json j
end

module GrantTokenList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module MessageType = struct
  type t =
    | RAW
    | DIGEST

  let str_to_t = [ "DIGEST", DIGEST; "RAW", RAW ]

  let t_to_str = [ DIGEST, "DIGEST"; RAW, "RAW" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module KeyMetadata = struct
  type t =
    { a_w_s_account_id : String.t option
    ; key_id : String.t
    ; arn : String.t option
    ; creation_date : DateTime.t option
    ; enabled : Boolean.t option
    ; description : String.t option
    ; key_usage : KeyUsageType.t option
    ; key_state : KeyState.t option
    ; deletion_date : DateTime.t option
    ; valid_to : DateTime.t option
    ; origin : OriginType.t option
    ; custom_key_store_id : String.t option
    ; cloud_hsm_cluster_id : String.t option
    ; expiration_model : ExpirationModelType.t option
    ; key_manager : KeyManagerType.t option
    ; customer_master_key_spec : CustomerMasterKeySpec.t option
    ; key_spec : KeySpec.t option
    ; encryption_algorithms : EncryptionAlgorithmSpecList.t
    ; signing_algorithms : SigningAlgorithmSpecList.t
    ; multi_region : Boolean.t option
    ; multi_region_configuration : MultiRegionConfiguration.t option
    ; pending_deletion_window_in_days : Integer.t option
    }

  let make
      ?a_w_s_account_id
      ~key_id
      ?arn
      ?creation_date
      ?enabled
      ?description
      ?key_usage
      ?key_state
      ?deletion_date
      ?valid_to
      ?origin
      ?custom_key_store_id
      ?cloud_hsm_cluster_id
      ?expiration_model
      ?key_manager
      ?customer_master_key_spec
      ?key_spec
      ?(encryption_algorithms = [])
      ?(signing_algorithms = [])
      ?multi_region
      ?multi_region_configuration
      ?pending_deletion_window_in_days
      () =
    { a_w_s_account_id
    ; key_id
    ; arn
    ; creation_date
    ; enabled
    ; description
    ; key_usage
    ; key_state
    ; deletion_date
    ; valid_to
    ; origin
    ; custom_key_store_id
    ; cloud_hsm_cluster_id
    ; expiration_model
    ; key_manager
    ; customer_master_key_spec
    ; key_spec
    ; encryption_algorithms
    ; signing_algorithms
    ; multi_region
    ; multi_region_configuration
    ; pending_deletion_window_in_days
    }

  let parse xml =
    Some
      { a_w_s_account_id = Util.option_bind (Xml.member "AWSAccountId" xml) String.parse
      ; key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; arn = Util.option_bind (Xml.member "Arn" xml) String.parse
      ; creation_date = Util.option_bind (Xml.member "CreationDate" xml) DateTime.parse
      ; enabled = Util.option_bind (Xml.member "Enabled" xml) Boolean.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; key_usage = Util.option_bind (Xml.member "KeyUsage" xml) KeyUsageType.parse
      ; key_state = Util.option_bind (Xml.member "KeyState" xml) KeyState.parse
      ; deletion_date = Util.option_bind (Xml.member "DeletionDate" xml) DateTime.parse
      ; valid_to = Util.option_bind (Xml.member "ValidTo" xml) DateTime.parse
      ; origin = Util.option_bind (Xml.member "Origin" xml) OriginType.parse
      ; custom_key_store_id =
          Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse
      ; cloud_hsm_cluster_id =
          Util.option_bind (Xml.member "CloudHsmClusterId" xml) String.parse
      ; expiration_model =
          Util.option_bind (Xml.member "ExpirationModel" xml) ExpirationModelType.parse
      ; key_manager = Util.option_bind (Xml.member "KeyManager" xml) KeyManagerType.parse
      ; customer_master_key_spec =
          Util.option_bind
            (Xml.member "CustomerMasterKeySpec" xml)
            CustomerMasterKeySpec.parse
      ; key_spec = Util.option_bind (Xml.member "KeySpec" xml) KeySpec.parse
      ; encryption_algorithms =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EncryptionAlgorithms" xml)
               EncryptionAlgorithmSpecList.parse)
      ; signing_algorithms =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SigningAlgorithms" xml)
               SigningAlgorithmSpecList.parse)
      ; multi_region = Util.option_bind (Xml.member "MultiRegion" xml) Boolean.parse
      ; multi_region_configuration =
          Util.option_bind
            (Xml.member "MultiRegionConfiguration" xml)
            MultiRegionConfiguration.parse
      ; pending_deletion_window_in_days =
          Util.option_bind (Xml.member "PendingDeletionWindowInDays" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.pending_deletion_window_in_days (fun f ->
               Query.Pair ("PendingDeletionWindowInDays", Integer.to_query f))
         ; Util.option_map v.multi_region_configuration (fun f ->
               Query.Pair ("MultiRegionConfiguration", MultiRegionConfiguration.to_query f))
         ; Util.option_map v.multi_region (fun f ->
               Query.Pair ("MultiRegion", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "SigningAlgorithms.member"
                , SigningAlgorithmSpecList.to_query v.signing_algorithms ))
         ; Some
             (Query.Pair
                ( "EncryptionAlgorithms.member"
                , EncryptionAlgorithmSpecList.to_query v.encryption_algorithms ))
         ; Util.option_map v.key_spec (fun f ->
               Query.Pair ("KeySpec", KeySpec.to_query f))
         ; Util.option_map v.customer_master_key_spec (fun f ->
               Query.Pair ("CustomerMasterKeySpec", CustomerMasterKeySpec.to_query f))
         ; Util.option_map v.key_manager (fun f ->
               Query.Pair ("KeyManager", KeyManagerType.to_query f))
         ; Util.option_map v.expiration_model (fun f ->
               Query.Pair ("ExpirationModel", ExpirationModelType.to_query f))
         ; Util.option_map v.cloud_hsm_cluster_id (fun f ->
               Query.Pair ("CloudHsmClusterId", String.to_query f))
         ; Util.option_map v.custom_key_store_id (fun f ->
               Query.Pair ("CustomKeyStoreId", String.to_query f))
         ; Util.option_map v.origin (fun f ->
               Query.Pair ("Origin", OriginType.to_query f))
         ; Util.option_map v.valid_to (fun f ->
               Query.Pair ("ValidTo", DateTime.to_query f))
         ; Util.option_map v.deletion_date (fun f ->
               Query.Pair ("DeletionDate", DateTime.to_query f))
         ; Util.option_map v.key_state (fun f ->
               Query.Pair ("KeyState", KeyState.to_query f))
         ; Util.option_map v.key_usage (fun f ->
               Query.Pair ("KeyUsage", KeyUsageType.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.enabled (fun f -> Query.Pair ("Enabled", Boolean.to_query f))
         ; Util.option_map v.creation_date (fun f ->
               Query.Pair ("CreationDate", DateTime.to_query f))
         ; Util.option_map v.arn (fun f -> Query.Pair ("Arn", String.to_query f))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ; Util.option_map v.a_w_s_account_id (fun f ->
               Query.Pair ("AWSAccountId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.pending_deletion_window_in_days (fun f ->
               "pending_deletion_window_in_days", Integer.to_json f)
         ; Util.option_map v.multi_region_configuration (fun f ->
               "multi_region_configuration", MultiRegionConfiguration.to_json f)
         ; Util.option_map v.multi_region (fun f -> "multi_region", Boolean.to_json f)
         ; Some
             ("signing_algorithms", SigningAlgorithmSpecList.to_json v.signing_algorithms)
         ; Some
             ( "encryption_algorithms"
             , EncryptionAlgorithmSpecList.to_json v.encryption_algorithms )
         ; Util.option_map v.key_spec (fun f -> "key_spec", KeySpec.to_json f)
         ; Util.option_map v.customer_master_key_spec (fun f ->
               "customer_master_key_spec", CustomerMasterKeySpec.to_json f)
         ; Util.option_map v.key_manager (fun f ->
               "key_manager", KeyManagerType.to_json f)
         ; Util.option_map v.expiration_model (fun f ->
               "expiration_model", ExpirationModelType.to_json f)
         ; Util.option_map v.cloud_hsm_cluster_id (fun f ->
               "cloud_hsm_cluster_id", String.to_json f)
         ; Util.option_map v.custom_key_store_id (fun f ->
               "custom_key_store_id", String.to_json f)
         ; Util.option_map v.origin (fun f -> "origin", OriginType.to_json f)
         ; Util.option_map v.valid_to (fun f -> "valid_to", DateTime.to_json f)
         ; Util.option_map v.deletion_date (fun f -> "deletion_date", DateTime.to_json f)
         ; Util.option_map v.key_state (fun f -> "key_state", KeyState.to_json f)
         ; Util.option_map v.key_usage (fun f -> "key_usage", KeyUsageType.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.enabled (fun f -> "enabled", Boolean.to_json f)
         ; Util.option_map v.creation_date (fun f -> "creation_date", DateTime.to_json f)
         ; Util.option_map v.arn (fun f -> "arn", String.to_json f)
         ; Some ("key_id", String.to_json v.key_id)
         ; Util.option_map v.a_w_s_account_id (fun f ->
               "a_w_s_account_id", String.to_json f)
         ])

  let of_json j =
    { a_w_s_account_id = Util.option_map (Json.lookup j "a_w_s_account_id") String.of_json
    ; key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; arn = Util.option_map (Json.lookup j "arn") String.of_json
    ; creation_date = Util.option_map (Json.lookup j "creation_date") DateTime.of_json
    ; enabled = Util.option_map (Json.lookup j "enabled") Boolean.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; key_usage = Util.option_map (Json.lookup j "key_usage") KeyUsageType.of_json
    ; key_state = Util.option_map (Json.lookup j "key_state") KeyState.of_json
    ; deletion_date = Util.option_map (Json.lookup j "deletion_date") DateTime.of_json
    ; valid_to = Util.option_map (Json.lookup j "valid_to") DateTime.of_json
    ; origin = Util.option_map (Json.lookup j "origin") OriginType.of_json
    ; custom_key_store_id =
        Util.option_map (Json.lookup j "custom_key_store_id") String.of_json
    ; cloud_hsm_cluster_id =
        Util.option_map (Json.lookup j "cloud_hsm_cluster_id") String.of_json
    ; expiration_model =
        Util.option_map (Json.lookup j "expiration_model") ExpirationModelType.of_json
    ; key_manager = Util.option_map (Json.lookup j "key_manager") KeyManagerType.of_json
    ; customer_master_key_spec =
        Util.option_map
          (Json.lookup j "customer_master_key_spec")
          CustomerMasterKeySpec.of_json
    ; key_spec = Util.option_map (Json.lookup j "key_spec") KeySpec.of_json
    ; encryption_algorithms =
        EncryptionAlgorithmSpecList.of_json
          (Util.of_option_exn (Json.lookup j "encryption_algorithms"))
    ; signing_algorithms =
        SigningAlgorithmSpecList.of_json
          (Util.of_option_exn (Json.lookup j "signing_algorithms"))
    ; multi_region = Util.option_map (Json.lookup j "multi_region") Boolean.of_json
    ; multi_region_configuration =
        Util.option_map
          (Json.lookup j "multi_region_configuration")
          MultiRegionConfiguration.of_json
    ; pending_deletion_window_in_days =
        Util.option_map (Json.lookup j "pending_deletion_window_in_days") Integer.of_json
    }
end

module DataKeySpec = struct
  type t =
    | AES_256
    | AES_128

  let str_to_t = [ "AES_128", AES_128; "AES_256", AES_256 ]

  let t_to_str = [ AES_128, "AES_128"; AES_256, "AES_256" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module CustomKeyStoresList = struct
  type t = CustomKeyStoresListEntry.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map CustomKeyStoresListEntry.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list CustomKeyStoresListEntry.to_query v

  let to_json v = `List (List.map CustomKeyStoresListEntry.to_json v)

  let of_json j = Json.to_list CustomKeyStoresListEntry.of_json j
end

module DataKeyPairSpec = struct
  type t =
    | RSA_2048
    | RSA_3072
    | RSA_4096
    | ECC_NIST_P256
    | ECC_NIST_P384
    | ECC_NIST_P521
    | ECC_SECG_P256K1

  let str_to_t =
    [ "ECC_SECG_P256K1", ECC_SECG_P256K1
    ; "ECC_NIST_P521", ECC_NIST_P521
    ; "ECC_NIST_P384", ECC_NIST_P384
    ; "ECC_NIST_P256", ECC_NIST_P256
    ; "RSA_4096", RSA_4096
    ; "RSA_3072", RSA_3072
    ; "RSA_2048", RSA_2048
    ]

  let t_to_str =
    [ ECC_SECG_P256K1, "ECC_SECG_P256K1"
    ; ECC_NIST_P521, "ECC_NIST_P521"
    ; ECC_NIST_P384, "ECC_NIST_P384"
    ; ECC_NIST_P256, "ECC_NIST_P256"
    ; RSA_4096, "RSA_4096"
    ; RSA_3072, "RSA_3072"
    ; RSA_2048, "RSA_2048"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module PolicyNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module GrantList = struct
  type t = GrantListEntry.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map GrantListEntry.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list GrantListEntry.to_query v

  let to_json v = `List (List.map GrantListEntry.to_json v)

  let of_json j = Json.to_list GrantListEntry.of_json j
end

module AliasList = struct
  type t = AliasListEntry.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map AliasListEntry.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list AliasListEntry.to_query v

  let to_json v = `List (List.map AliasListEntry.to_json v)

  let of_json j = Json.to_list AliasListEntry.of_json j
end

module TagKeyList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module InvalidImportTokenException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module UpdateCustomKeyStoreResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeCustomKeyStoresRequest = struct
  type t =
    { custom_key_store_id : String.t option
    ; custom_key_store_name : String.t option
    ; limit : Integer.t option
    ; marker : String.t option
    }

  let make ?custom_key_store_id ?custom_key_store_name ?limit ?marker () =
    { custom_key_store_id; custom_key_store_name; limit; marker }

  let parse xml =
    Some
      { custom_key_store_id =
          Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse
      ; custom_key_store_name =
          Util.option_bind (Xml.member "CustomKeyStoreName" xml) String.parse
      ; limit = Util.option_bind (Xml.member "Limit" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.limit (fun f -> Query.Pair ("Limit", Integer.to_query f))
         ; Util.option_map v.custom_key_store_name (fun f ->
               Query.Pair ("CustomKeyStoreName", String.to_query f))
         ; Util.option_map v.custom_key_store_id (fun f ->
               Query.Pair ("CustomKeyStoreId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.limit (fun f -> "limit", Integer.to_json f)
         ; Util.option_map v.custom_key_store_name (fun f ->
               "custom_key_store_name", String.to_json f)
         ; Util.option_map v.custom_key_store_id (fun f ->
               "custom_key_store_id", String.to_json f)
         ])

  let of_json j =
    { custom_key_store_id =
        Util.option_map (Json.lookup j "custom_key_store_id") String.of_json
    ; custom_key_store_name =
        Util.option_map (Json.lookup j "custom_key_store_name") String.of_json
    ; limit = Util.option_map (Json.lookup j "limit") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module ConnectCustomKeyStoreRequest = struct
  type t = { custom_key_store_id : String.t }

  let make ~custom_key_store_id () = { custom_key_store_id }

  let parse xml =
    Some
      { custom_key_store_id =
          Xml.required
            "CustomKeyStoreId"
            (Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("custom_key_store_id", String.to_json v.custom_key_store_id) ])

  let of_json j =
    { custom_key_store_id =
        String.of_json (Util.of_option_exn (Json.lookup j "custom_key_store_id"))
    }
end

module ListResourceTagsResponse = struct
  type t =
    { tags : TagList.t
    ; next_marker : String.t option
    ; truncated : Boolean.t option
    }

  let make ?(tags = []) ?next_marker ?truncated () = { tags; next_marker; truncated }

  let parse xml =
    Some
      { tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; next_marker = Util.option_bind (Xml.member "NextMarker" xml) String.parse
      ; truncated = Util.option_bind (Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f ->
               Query.Pair ("Truncated", Boolean.to_query f))
         ; Util.option_map v.next_marker (fun f ->
               Query.Pair ("NextMarker", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f -> "truncated", Boolean.to_json f)
         ; Util.option_map v.next_marker (fun f -> "next_marker", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ])

  let of_json j =
    { tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; next_marker = Util.option_map (Json.lookup j "next_marker") String.of_json
    ; truncated = Util.option_map (Json.lookup j "truncated") Boolean.of_json
    }
end

module CloudHsmClusterInUseException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module CustomKeyStoreHasCMKsException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module DisconnectCustomKeyStoreResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module GetParametersForImportRequest = struct
  type t =
    { key_id : String.t
    ; wrapping_algorithm : AlgorithmSpec.t
    ; wrapping_key_spec : WrappingKeySpec.t
    }

  let make ~key_id ~wrapping_algorithm ~wrapping_key_spec () =
    { key_id; wrapping_algorithm; wrapping_key_spec }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; wrapping_algorithm =
          Xml.required
            "WrappingAlgorithm"
            (Util.option_bind (Xml.member "WrappingAlgorithm" xml) AlgorithmSpec.parse)
      ; wrapping_key_spec =
          Xml.required
            "WrappingKeySpec"
            (Util.option_bind (Xml.member "WrappingKeySpec" xml) WrappingKeySpec.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("WrappingKeySpec", WrappingKeySpec.to_query v.wrapping_key_spec))
         ; Some
             (Query.Pair ("WrappingAlgorithm", AlgorithmSpec.to_query v.wrapping_algorithm))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("wrapping_key_spec", WrappingKeySpec.to_json v.wrapping_key_spec)
         ; Some ("wrapping_algorithm", AlgorithmSpec.to_json v.wrapping_algorithm)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; wrapping_algorithm =
        AlgorithmSpec.of_json (Util.of_option_exn (Json.lookup j "wrapping_algorithm"))
    ; wrapping_key_spec =
        WrappingKeySpec.of_json (Util.of_option_exn (Json.lookup j "wrapping_key_spec"))
    }
end

module GetKeyPolicyResponse = struct
  type t = { policy : String.t option }

  let make ?policy () = { policy }

  let parse xml =
    Some { policy = Util.option_bind (Xml.member "Policy" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.policy (fun f -> Query.Pair ("Policy", String.to_query f)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.policy (fun f -> "policy", String.to_json f) ])

  let of_json j = { policy = Util.option_map (Json.lookup j "policy") String.of_json }
end

module InvalidCiphertextException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListKeysResponse = struct
  type t =
    { keys : KeyList.t
    ; next_marker : String.t option
    ; truncated : Boolean.t option
    }

  let make ?(keys = []) ?next_marker ?truncated () = { keys; next_marker; truncated }

  let parse xml =
    Some
      { keys = Util.of_option [] (Util.option_bind (Xml.member "Keys" xml) KeyList.parse)
      ; next_marker = Util.option_bind (Xml.member "NextMarker" xml) String.parse
      ; truncated = Util.option_bind (Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f ->
               Query.Pair ("Truncated", Boolean.to_query f))
         ; Util.option_map v.next_marker (fun f ->
               Query.Pair ("NextMarker", String.to_query f))
         ; Some (Query.Pair ("Keys.member", KeyList.to_query v.keys))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f -> "truncated", Boolean.to_json f)
         ; Util.option_map v.next_marker (fun f -> "next_marker", String.to_json f)
         ; Some ("keys", KeyList.to_json v.keys)
         ])

  let of_json j =
    { keys = KeyList.of_json (Util.of_option_exn (Json.lookup j "keys"))
    ; next_marker = Util.option_map (Json.lookup j "next_marker") String.of_json
    ; truncated = Util.option_map (Json.lookup j "truncated") Boolean.of_json
    }
end

module ImportKeyMaterialResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module TagResourceRequest = struct
  type t =
    { key_id : String.t
    ; tags : TagList.t
    }

  let make ~key_id ~tags () = { key_id; tags }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; tags =
          Xml.required "Tags" (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module VerifyRequest = struct
  type t =
    { key_id : String.t
    ; message : Blob.t
    ; message_type : MessageType.t option
    ; signature : Blob.t
    ; signing_algorithm : SigningAlgorithmSpec.t
    ; grant_tokens : GrantTokenList.t
    }

  let make
      ~key_id
      ~message
      ?message_type
      ~signature
      ~signing_algorithm
      ?(grant_tokens = [])
      () =
    { key_id; message; message_type; signature; signing_algorithm; grant_tokens }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; message =
          Xml.required "Message" (Util.option_bind (Xml.member "Message" xml) Blob.parse)
      ; message_type = Util.option_bind (Xml.member "MessageType" xml) MessageType.parse
      ; signature =
          Xml.required
            "Signature"
            (Util.option_bind (Xml.member "Signature" xml) Blob.parse)
      ; signing_algorithm =
          Xml.required
            "SigningAlgorithm"
            (Util.option_bind
               (Xml.member "SigningAlgorithm" xml)
               SigningAlgorithmSpec.parse)
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some
             (Query.Pair
                ("SigningAlgorithm", SigningAlgorithmSpec.to_query v.signing_algorithm))
         ; Some (Query.Pair ("Signature", Blob.to_query v.signature))
         ; Util.option_map v.message_type (fun f ->
               Query.Pair ("MessageType", MessageType.to_query f))
         ; Some (Query.Pair ("Message", Blob.to_query v.message))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("signing_algorithm", SigningAlgorithmSpec.to_json v.signing_algorithm)
         ; Some ("signature", Blob.to_json v.signature)
         ; Util.option_map v.message_type (fun f -> "message_type", MessageType.to_json f)
         ; Some ("message", Blob.to_json v.message)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; message = Blob.of_json (Util.of_option_exn (Json.lookup j "message"))
    ; message_type = Util.option_map (Json.lookup j "message_type") MessageType.of_json
    ; signature = Blob.of_json (Util.of_option_exn (Json.lookup j "signature"))
    ; signing_algorithm =
        SigningAlgorithmSpec.of_json
          (Util.of_option_exn (Json.lookup j "signing_algorithm"))
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    }
end

module KMSInvalidStateException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module CloudHsmClusterNotActiveException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module DescribeKeyResponse = struct
  type t = { key_metadata : KeyMetadata.t option }

  let make ?key_metadata () = { key_metadata }

  let parse xml =
    Some
      { key_metadata = Util.option_bind (Xml.member "KeyMetadata" xml) KeyMetadata.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.key_metadata (fun f ->
               Query.Pair ("KeyMetadata", KeyMetadata.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.key_metadata (fun f -> "key_metadata", KeyMetadata.to_json f)
         ])

  let of_json j =
    { key_metadata = Util.option_map (Json.lookup j "key_metadata") KeyMetadata.of_json }
end

module EncryptRequest = struct
  type t =
    { key_id : String.t
    ; plaintext : Blob.t
    ; encryption_context : EncryptionContextType.t option
    ; grant_tokens : GrantTokenList.t
    ; encryption_algorithm : EncryptionAlgorithmSpec.t option
    }

  let make
      ~key_id
      ~plaintext
      ?encryption_context
      ?(grant_tokens = [])
      ?encryption_algorithm
      () =
    { key_id; plaintext; encryption_context; grant_tokens; encryption_algorithm }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; plaintext =
          Xml.required
            "Plaintext"
            (Util.option_bind (Xml.member "Plaintext" xml) Blob.parse)
      ; encryption_context =
          Util.option_bind
            (Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      ; encryption_algorithm =
          Util.option_bind
            (Xml.member "EncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.encryption_algorithm (fun f ->
               Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Util.option_map v.encryption_context (fun f ->
               Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ; Some (Query.Pair ("Plaintext", Blob.to_query v.plaintext))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.encryption_algorithm (fun f ->
               "encryption_algorithm", EncryptionAlgorithmSpec.to_json f)
         ; Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Util.option_map v.encryption_context (fun f ->
               "encryption_context", EncryptionContextType.to_json f)
         ; Some ("plaintext", Blob.to_json v.plaintext)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; plaintext = Blob.of_json (Util.of_option_exn (Json.lookup j "plaintext"))
    ; encryption_context =
        Util.option_map (Json.lookup j "encryption_context") EncryptionContextType.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    ; encryption_algorithm =
        Util.option_map
          (Json.lookup j "encryption_algorithm")
          EncryptionAlgorithmSpec.of_json
    }
end

module KMSInvalidSignatureException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module UpdateCustomKeyStoreRequest = struct
  type t =
    { custom_key_store_id : String.t
    ; new_custom_key_store_name : String.t option
    ; key_store_password : String.t option
    ; cloud_hsm_cluster_id : String.t option
    }

  let make
      ~custom_key_store_id
      ?new_custom_key_store_name
      ?key_store_password
      ?cloud_hsm_cluster_id
      () =
    { custom_key_store_id
    ; new_custom_key_store_name
    ; key_store_password
    ; cloud_hsm_cluster_id
    }

  let parse xml =
    Some
      { custom_key_store_id =
          Xml.required
            "CustomKeyStoreId"
            (Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse)
      ; new_custom_key_store_name =
          Util.option_bind (Xml.member "NewCustomKeyStoreName" xml) String.parse
      ; key_store_password =
          Util.option_bind (Xml.member "KeyStorePassword" xml) String.parse
      ; cloud_hsm_cluster_id =
          Util.option_bind (Xml.member "CloudHsmClusterId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.cloud_hsm_cluster_id (fun f ->
               Query.Pair ("CloudHsmClusterId", String.to_query f))
         ; Util.option_map v.key_store_password (fun f ->
               Query.Pair ("KeyStorePassword", String.to_query f))
         ; Util.option_map v.new_custom_key_store_name (fun f ->
               Query.Pair ("NewCustomKeyStoreName", String.to_query f))
         ; Some (Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.cloud_hsm_cluster_id (fun f ->
               "cloud_hsm_cluster_id", String.to_json f)
         ; Util.option_map v.key_store_password (fun f ->
               "key_store_password", String.to_json f)
         ; Util.option_map v.new_custom_key_store_name (fun f ->
               "new_custom_key_store_name", String.to_json f)
         ; Some ("custom_key_store_id", String.to_json v.custom_key_store_id)
         ])

  let of_json j =
    { custom_key_store_id =
        String.of_json (Util.of_option_exn (Json.lookup j "custom_key_store_id"))
    ; new_custom_key_store_name =
        Util.option_map (Json.lookup j "new_custom_key_store_name") String.of_json
    ; key_store_password =
        Util.option_map (Json.lookup j "key_store_password") String.of_json
    ; cloud_hsm_cluster_id =
        Util.option_map (Json.lookup j "cloud_hsm_cluster_id") String.of_json
    }
end

module ListRetirableGrantsRequest = struct
  type t =
    { limit : Integer.t option
    ; marker : String.t option
    ; retiring_principal : String.t
    }

  let make ?limit ?marker ~retiring_principal () = { limit; marker; retiring_principal }

  let parse xml =
    Some
      { limit = Util.option_bind (Xml.member "Limit" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; retiring_principal =
          Xml.required
            "RetiringPrincipal"
            (Util.option_bind (Xml.member "RetiringPrincipal" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("RetiringPrincipal", String.to_query v.retiring_principal))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.limit (fun f -> Query.Pair ("Limit", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("retiring_principal", String.to_json v.retiring_principal)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.limit (fun f -> "limit", Integer.to_json f)
         ])

  let of_json j =
    { limit = Util.option_map (Json.lookup j "limit") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; retiring_principal =
        String.of_json (Util.of_option_exn (Json.lookup j "retiring_principal"))
    }
end

module RetireGrantRequest = struct
  type t =
    { grant_token : String.t option
    ; key_id : String.t option
    ; grant_id : String.t option
    }

  let make ?grant_token ?key_id ?grant_id () = { grant_token; key_id; grant_id }

  let parse xml =
    Some
      { grant_token = Util.option_bind (Xml.member "GrantToken" xml) String.parse
      ; key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; grant_id = Util.option_bind (Xml.member "GrantId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.grant_id (fun f -> Query.Pair ("GrantId", String.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ; Util.option_map v.grant_token (fun f ->
               Query.Pair ("GrantToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.grant_id (fun f -> "grant_id", String.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ; Util.option_map v.grant_token (fun f -> "grant_token", String.to_json f)
         ])

  let of_json j =
    { grant_token = Util.option_map (Json.lookup j "grant_token") String.of_json
    ; key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; grant_id = Util.option_map (Json.lookup j "grant_id") String.of_json
    }
end

module CreateCustomKeyStoreResponse = struct
  type t = { custom_key_store_id : String.t option }

  let make ?custom_key_store_id () = { custom_key_store_id }

  let parse xml =
    Some
      { custom_key_store_id =
          Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.custom_key_store_id (fun f ->
               Query.Pair ("CustomKeyStoreId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.custom_key_store_id (fun f ->
               "custom_key_store_id", String.to_json f)
         ])

  let of_json j =
    { custom_key_store_id =
        Util.option_map (Json.lookup j "custom_key_store_id") String.of_json
    }
end

module ExpiredImportTokenException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GenerateRandomResponse = struct
  type t = { plaintext : Blob.t option }

  let make ?plaintext () = { plaintext }

  let parse xml =
    Some { plaintext = Util.option_bind (Xml.member "Plaintext" xml) Blob.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.plaintext (fun f ->
               Query.Pair ("Plaintext", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.plaintext (fun f -> "plaintext", Blob.to_json f) ])

  let of_json j = { plaintext = Util.option_map (Json.lookup j "plaintext") Blob.of_json }
end

module GenerateDataKeyRequest = struct
  type t =
    { key_id : String.t
    ; encryption_context : EncryptionContextType.t option
    ; number_of_bytes : Integer.t option
    ; key_spec : DataKeySpec.t option
    ; grant_tokens : GrantTokenList.t
    }

  let make ~key_id ?encryption_context ?number_of_bytes ?key_spec ?(grant_tokens = []) ()
      =
    { key_id; encryption_context; number_of_bytes; key_spec; grant_tokens }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; encryption_context =
          Util.option_bind
            (Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; number_of_bytes = Util.option_bind (Xml.member "NumberOfBytes" xml) Integer.parse
      ; key_spec = Util.option_bind (Xml.member "KeySpec" xml) DataKeySpec.parse
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Util.option_map v.key_spec (fun f ->
               Query.Pair ("KeySpec", DataKeySpec.to_query f))
         ; Util.option_map v.number_of_bytes (fun f ->
               Query.Pair ("NumberOfBytes", Integer.to_query f))
         ; Util.option_map v.encryption_context (fun f ->
               Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Util.option_map v.key_spec (fun f -> "key_spec", DataKeySpec.to_json f)
         ; Util.option_map v.number_of_bytes (fun f ->
               "number_of_bytes", Integer.to_json f)
         ; Util.option_map v.encryption_context (fun f ->
               "encryption_context", EncryptionContextType.to_json f)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; encryption_context =
        Util.option_map (Json.lookup j "encryption_context") EncryptionContextType.of_json
    ; number_of_bytes = Util.option_map (Json.lookup j "number_of_bytes") Integer.of_json
    ; key_spec = Util.option_map (Json.lookup j "key_spec") DataKeySpec.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    }
end

module DescribeCustomKeyStoresResponse = struct
  type t =
    { custom_key_stores : CustomKeyStoresList.t
    ; next_marker : String.t option
    ; truncated : Boolean.t option
    }

  let make ?(custom_key_stores = []) ?next_marker ?truncated () =
    { custom_key_stores; next_marker; truncated }

  let parse xml =
    Some
      { custom_key_stores =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "CustomKeyStores" xml)
               CustomKeyStoresList.parse)
      ; next_marker = Util.option_bind (Xml.member "NextMarker" xml) String.parse
      ; truncated = Util.option_bind (Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f ->
               Query.Pair ("Truncated", Boolean.to_query f))
         ; Util.option_map v.next_marker (fun f ->
               Query.Pair ("NextMarker", String.to_query f))
         ; Some
             (Query.Pair
                ( "CustomKeyStores.member"
                , CustomKeyStoresList.to_query v.custom_key_stores ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f -> "truncated", Boolean.to_json f)
         ; Util.option_map v.next_marker (fun f -> "next_marker", String.to_json f)
         ; Some ("custom_key_stores", CustomKeyStoresList.to_json v.custom_key_stores)
         ])

  let of_json j =
    { custom_key_stores =
        CustomKeyStoresList.of_json
          (Util.of_option_exn (Json.lookup j "custom_key_stores"))
    ; next_marker = Util.option_map (Json.lookup j "next_marker") String.of_json
    ; truncated = Util.option_map (Json.lookup j "truncated") Boolean.of_json
    }
end

module EncryptResponse = struct
  type t =
    { ciphertext_blob : Blob.t option
    ; key_id : String.t option
    ; encryption_algorithm : EncryptionAlgorithmSpec.t option
    }

  let make ?ciphertext_blob ?key_id ?encryption_algorithm () =
    { ciphertext_blob; key_id; encryption_algorithm }

  let parse xml =
    Some
      { ciphertext_blob = Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse
      ; key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; encryption_algorithm =
          Util.option_bind
            (Xml.member "EncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.encryption_algorithm (fun f ->
               Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ; Util.option_map v.ciphertext_blob (fun f ->
               Query.Pair ("CiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.encryption_algorithm (fun f ->
               "encryption_algorithm", EncryptionAlgorithmSpec.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ; Util.option_map v.ciphertext_blob (fun f -> "ciphertext_blob", Blob.to_json f)
         ])

  let of_json j =
    { ciphertext_blob = Util.option_map (Json.lookup j "ciphertext_blob") Blob.of_json
    ; key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; encryption_algorithm =
        Util.option_map
          (Json.lookup j "encryption_algorithm")
          EncryptionAlgorithmSpec.of_json
    }
end

module GenerateRandomRequest = struct
  type t =
    { number_of_bytes : Integer.t option
    ; custom_key_store_id : String.t option
    }

  let make ?number_of_bytes ?custom_key_store_id () =
    { number_of_bytes; custom_key_store_id }

  let parse xml =
    Some
      { number_of_bytes = Util.option_bind (Xml.member "NumberOfBytes" xml) Integer.parse
      ; custom_key_store_id =
          Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.custom_key_store_id (fun f ->
               Query.Pair ("CustomKeyStoreId", String.to_query f))
         ; Util.option_map v.number_of_bytes (fun f ->
               Query.Pair ("NumberOfBytes", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.custom_key_store_id (fun f ->
               "custom_key_store_id", String.to_json f)
         ; Util.option_map v.number_of_bytes (fun f ->
               "number_of_bytes", Integer.to_json f)
         ])

  let of_json j =
    { number_of_bytes = Util.option_map (Json.lookup j "number_of_bytes") Integer.of_json
    ; custom_key_store_id =
        Util.option_map (Json.lookup j "custom_key_store_id") String.of_json
    }
end

module DependencyTimeoutException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module CustomKeyStoreNotFoundException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module InvalidArnException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module CustomKeyStoreInvalidStateException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ImportKeyMaterialRequest = struct
  type t =
    { key_id : String.t
    ; import_token : Blob.t
    ; encrypted_key_material : Blob.t
    ; valid_to : DateTime.t option
    ; expiration_model : ExpirationModelType.t option
    }

  let make ~key_id ~import_token ~encrypted_key_material ?valid_to ?expiration_model () =
    { key_id; import_token; encrypted_key_material; valid_to; expiration_model }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; import_token =
          Xml.required
            "ImportToken"
            (Util.option_bind (Xml.member "ImportToken" xml) Blob.parse)
      ; encrypted_key_material =
          Xml.required
            "EncryptedKeyMaterial"
            (Util.option_bind (Xml.member "EncryptedKeyMaterial" xml) Blob.parse)
      ; valid_to = Util.option_bind (Xml.member "ValidTo" xml) DateTime.parse
      ; expiration_model =
          Util.option_bind (Xml.member "ExpirationModel" xml) ExpirationModelType.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.expiration_model (fun f ->
               Query.Pair ("ExpirationModel", ExpirationModelType.to_query f))
         ; Util.option_map v.valid_to (fun f ->
               Query.Pair ("ValidTo", DateTime.to_query f))
         ; Some
             (Query.Pair ("EncryptedKeyMaterial", Blob.to_query v.encrypted_key_material))
         ; Some (Query.Pair ("ImportToken", Blob.to_query v.import_token))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.expiration_model (fun f ->
               "expiration_model", ExpirationModelType.to_json f)
         ; Util.option_map v.valid_to (fun f -> "valid_to", DateTime.to_json f)
         ; Some ("encrypted_key_material", Blob.to_json v.encrypted_key_material)
         ; Some ("import_token", Blob.to_json v.import_token)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; import_token = Blob.of_json (Util.of_option_exn (Json.lookup j "import_token"))
    ; encrypted_key_material =
        Blob.of_json (Util.of_option_exn (Json.lookup j "encrypted_key_material"))
    ; valid_to = Util.option_map (Json.lookup j "valid_to") DateTime.of_json
    ; expiration_model =
        Util.option_map (Json.lookup j "expiration_model") ExpirationModelType.of_json
    }
end

module EnableKeyRotationRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("key_id", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id")) }
end

module KeyUnavailableException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module MalformedPolicyDocumentException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ScheduleKeyDeletionRequest = struct
  type t =
    { key_id : String.t
    ; pending_window_in_days : Integer.t option
    }

  let make ~key_id ?pending_window_in_days () = { key_id; pending_window_in_days }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; pending_window_in_days =
          Util.option_bind (Xml.member "PendingWindowInDays" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.pending_window_in_days (fun f ->
               Query.Pair ("PendingWindowInDays", Integer.to_query f))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.pending_window_in_days (fun f ->
               "pending_window_in_days", Integer.to_json f)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; pending_window_in_days =
        Util.option_map (Json.lookup j "pending_window_in_days") Integer.of_json
    }
end

module GetPublicKeyRequest = struct
  type t =
    { key_id : String.t
    ; grant_tokens : GrantTokenList.t
    }

  let make ~key_id ?(grant_tokens = []) () = { key_id; grant_tokens }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    }
end

module GenerateDataKeyPairRequest = struct
  type t =
    { encryption_context : EncryptionContextType.t option
    ; key_id : String.t
    ; key_pair_spec : DataKeyPairSpec.t
    ; grant_tokens : GrantTokenList.t
    }

  let make ?encryption_context ~key_id ~key_pair_spec ?(grant_tokens = []) () =
    { encryption_context; key_id; key_pair_spec; grant_tokens }

  let parse xml =
    Some
      { encryption_context =
          Util.option_bind
            (Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; key_pair_spec =
          Xml.required
            "KeyPairSpec"
            (Util.option_bind (Xml.member "KeyPairSpec" xml) DataKeyPairSpec.parse)
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some (Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query v.key_pair_spec))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ; Util.option_map v.encryption_context (fun f ->
               Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("key_pair_spec", DataKeyPairSpec.to_json v.key_pair_spec)
         ; Some ("key_id", String.to_json v.key_id)
         ; Util.option_map v.encryption_context (fun f ->
               "encryption_context", EncryptionContextType.to_json f)
         ])

  let of_json j =
    { encryption_context =
        Util.option_map (Json.lookup j "encryption_context") EncryptionContextType.of_json
    ; key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; key_pair_spec =
        DataKeyPairSpec.of_json (Util.of_option_exn (Json.lookup j "key_pair_spec"))
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    }
end

module GetKeyRotationStatusRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("key_id", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id")) }
end

module ReEncryptRequest = struct
  type t =
    { ciphertext_blob : Blob.t
    ; source_encryption_context : EncryptionContextType.t option
    ; source_key_id : String.t option
    ; destination_key_id : String.t
    ; destination_encryption_context : EncryptionContextType.t option
    ; source_encryption_algorithm : EncryptionAlgorithmSpec.t option
    ; destination_encryption_algorithm : EncryptionAlgorithmSpec.t option
    ; grant_tokens : GrantTokenList.t
    }

  let make
      ~ciphertext_blob
      ?source_encryption_context
      ?source_key_id
      ~destination_key_id
      ?destination_encryption_context
      ?source_encryption_algorithm
      ?destination_encryption_algorithm
      ?(grant_tokens = [])
      () =
    { ciphertext_blob
    ; source_encryption_context
    ; source_key_id
    ; destination_key_id
    ; destination_encryption_context
    ; source_encryption_algorithm
    ; destination_encryption_algorithm
    ; grant_tokens
    }

  let parse xml =
    Some
      { ciphertext_blob =
          Xml.required
            "CiphertextBlob"
            (Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse)
      ; source_encryption_context =
          Util.option_bind
            (Xml.member "SourceEncryptionContext" xml)
            EncryptionContextType.parse
      ; source_key_id = Util.option_bind (Xml.member "SourceKeyId" xml) String.parse
      ; destination_key_id =
          Xml.required
            "DestinationKeyId"
            (Util.option_bind (Xml.member "DestinationKeyId" xml) String.parse)
      ; destination_encryption_context =
          Util.option_bind
            (Xml.member "DestinationEncryptionContext" xml)
            EncryptionContextType.parse
      ; source_encryption_algorithm =
          Util.option_bind
            (Xml.member "SourceEncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      ; destination_encryption_algorithm =
          Util.option_bind
            (Xml.member "DestinationEncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Util.option_map v.destination_encryption_algorithm (fun f ->
               Query.Pair
                 ("DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Util.option_map v.source_encryption_algorithm (fun f ->
               Query.Pair ("SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Util.option_map v.destination_encryption_context (fun f ->
               Query.Pair
                 ("DestinationEncryptionContext", EncryptionContextType.to_query f))
         ; Some (Query.Pair ("DestinationKeyId", String.to_query v.destination_key_id))
         ; Util.option_map v.source_key_id (fun f ->
               Query.Pair ("SourceKeyId", String.to_query f))
         ; Util.option_map v.source_encryption_context (fun f ->
               Query.Pair ("SourceEncryptionContext", EncryptionContextType.to_query f))
         ; Some (Query.Pair ("CiphertextBlob", Blob.to_query v.ciphertext_blob))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Util.option_map v.destination_encryption_algorithm (fun f ->
               "destination_encryption_algorithm", EncryptionAlgorithmSpec.to_json f)
         ; Util.option_map v.source_encryption_algorithm (fun f ->
               "source_encryption_algorithm", EncryptionAlgorithmSpec.to_json f)
         ; Util.option_map v.destination_encryption_context (fun f ->
               "destination_encryption_context", EncryptionContextType.to_json f)
         ; Some ("destination_key_id", String.to_json v.destination_key_id)
         ; Util.option_map v.source_key_id (fun f -> "source_key_id", String.to_json f)
         ; Util.option_map v.source_encryption_context (fun f ->
               "source_encryption_context", EncryptionContextType.to_json f)
         ; Some ("ciphertext_blob", Blob.to_json v.ciphertext_blob)
         ])

  let of_json j =
    { ciphertext_blob =
        Blob.of_json (Util.of_option_exn (Json.lookup j "ciphertext_blob"))
    ; source_encryption_context =
        Util.option_map
          (Json.lookup j "source_encryption_context")
          EncryptionContextType.of_json
    ; source_key_id = Util.option_map (Json.lookup j "source_key_id") String.of_json
    ; destination_key_id =
        String.of_json (Util.of_option_exn (Json.lookup j "destination_key_id"))
    ; destination_encryption_context =
        Util.option_map
          (Json.lookup j "destination_encryption_context")
          EncryptionContextType.of_json
    ; source_encryption_algorithm =
        Util.option_map
          (Json.lookup j "source_encryption_algorithm")
          EncryptionAlgorithmSpec.of_json
    ; destination_encryption_algorithm =
        Util.option_map
          (Json.lookup j "destination_encryption_algorithm")
          EncryptionAlgorithmSpec.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    }
end

module CustomKeyStoreNameInUseException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module InvalidMarkerException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module EnableKeyRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("key_id", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id")) }
end

module DeleteCustomKeyStoreRequest = struct
  type t = { custom_key_store_id : String.t }

  let make ~custom_key_store_id () = { custom_key_store_id }

  let parse xml =
    Some
      { custom_key_store_id =
          Xml.required
            "CustomKeyStoreId"
            (Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("custom_key_store_id", String.to_json v.custom_key_store_id) ])

  let of_json j =
    { custom_key_store_id =
        String.of_json (Util.of_option_exn (Json.lookup j "custom_key_store_id"))
    }
end

module DeleteImportedKeyMaterialRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("key_id", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id")) }
end

module IncorrectKeyException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetPublicKeyResponse = struct
  type t =
    { key_id : String.t option
    ; public_key : Blob.t option
    ; customer_master_key_spec : CustomerMasterKeySpec.t option
    ; key_spec : KeySpec.t option
    ; key_usage : KeyUsageType.t option
    ; encryption_algorithms : EncryptionAlgorithmSpecList.t
    ; signing_algorithms : SigningAlgorithmSpecList.t
    }

  let make
      ?key_id
      ?public_key
      ?customer_master_key_spec
      ?key_spec
      ?key_usage
      ?(encryption_algorithms = [])
      ?(signing_algorithms = [])
      () =
    { key_id
    ; public_key
    ; customer_master_key_spec
    ; key_spec
    ; key_usage
    ; encryption_algorithms
    ; signing_algorithms
    }

  let parse xml =
    Some
      { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; public_key = Util.option_bind (Xml.member "PublicKey" xml) Blob.parse
      ; customer_master_key_spec =
          Util.option_bind
            (Xml.member "CustomerMasterKeySpec" xml)
            CustomerMasterKeySpec.parse
      ; key_spec = Util.option_bind (Xml.member "KeySpec" xml) KeySpec.parse
      ; key_usage = Util.option_bind (Xml.member "KeyUsage" xml) KeyUsageType.parse
      ; encryption_algorithms =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EncryptionAlgorithms" xml)
               EncryptionAlgorithmSpecList.parse)
      ; signing_algorithms =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SigningAlgorithms" xml)
               SigningAlgorithmSpecList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "SigningAlgorithms.member"
                , SigningAlgorithmSpecList.to_query v.signing_algorithms ))
         ; Some
             (Query.Pair
                ( "EncryptionAlgorithms.member"
                , EncryptionAlgorithmSpecList.to_query v.encryption_algorithms ))
         ; Util.option_map v.key_usage (fun f ->
               Query.Pair ("KeyUsage", KeyUsageType.to_query f))
         ; Util.option_map v.key_spec (fun f ->
               Query.Pair ("KeySpec", KeySpec.to_query f))
         ; Util.option_map v.customer_master_key_spec (fun f ->
               Query.Pair ("CustomerMasterKeySpec", CustomerMasterKeySpec.to_query f))
         ; Util.option_map v.public_key (fun f ->
               Query.Pair ("PublicKey", Blob.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ("signing_algorithms", SigningAlgorithmSpecList.to_json v.signing_algorithms)
         ; Some
             ( "encryption_algorithms"
             , EncryptionAlgorithmSpecList.to_json v.encryption_algorithms )
         ; Util.option_map v.key_usage (fun f -> "key_usage", KeyUsageType.to_json f)
         ; Util.option_map v.key_spec (fun f -> "key_spec", KeySpec.to_json f)
         ; Util.option_map v.customer_master_key_spec (fun f ->
               "customer_master_key_spec", CustomerMasterKeySpec.to_json f)
         ; Util.option_map v.public_key (fun f -> "public_key", Blob.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ])

  let of_json j =
    { key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; public_key = Util.option_map (Json.lookup j "public_key") Blob.of_json
    ; customer_master_key_spec =
        Util.option_map
          (Json.lookup j "customer_master_key_spec")
          CustomerMasterKeySpec.of_json
    ; key_spec = Util.option_map (Json.lookup j "key_spec") KeySpec.of_json
    ; key_usage = Util.option_map (Json.lookup j "key_usage") KeyUsageType.of_json
    ; encryption_algorithms =
        EncryptionAlgorithmSpecList.of_json
          (Util.of_option_exn (Json.lookup j "encryption_algorithms"))
    ; signing_algorithms =
        SigningAlgorithmSpecList.of_json
          (Util.of_option_exn (Json.lookup j "signing_algorithms"))
    }
end

module CloudHsmClusterNotRelatedException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListResourceTagsRequest = struct
  type t =
    { key_id : String.t
    ; limit : Integer.t option
    ; marker : String.t option
    }

  let make ~key_id ?limit ?marker () = { key_id; limit; marker }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; limit = Util.option_bind (Xml.member "Limit" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.limit (fun f -> Query.Pair ("Limit", Integer.to_query f))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.limit (fun f -> "limit", Integer.to_json f)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; limit = Util.option_map (Json.lookup j "limit") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DeleteCustomKeyStoreResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DecryptResponse = struct
  type t =
    { key_id : String.t option
    ; plaintext : Blob.t option
    ; encryption_algorithm : EncryptionAlgorithmSpec.t option
    }

  let make ?key_id ?plaintext ?encryption_algorithm () =
    { key_id; plaintext; encryption_algorithm }

  let parse xml =
    Some
      { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; plaintext = Util.option_bind (Xml.member "Plaintext" xml) Blob.parse
      ; encryption_algorithm =
          Util.option_bind
            (Xml.member "EncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.encryption_algorithm (fun f ->
               Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Util.option_map v.plaintext (fun f ->
               Query.Pair ("Plaintext", Blob.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.encryption_algorithm (fun f ->
               "encryption_algorithm", EncryptionAlgorithmSpec.to_json f)
         ; Util.option_map v.plaintext (fun f -> "plaintext", Blob.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ])

  let of_json j =
    { key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; plaintext = Util.option_map (Json.lookup j "plaintext") Blob.of_json
    ; encryption_algorithm =
        Util.option_map
          (Json.lookup j "encryption_algorithm")
          EncryptionAlgorithmSpec.of_json
    }
end

module KMSInternalException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module SignResponse = struct
  type t =
    { key_id : String.t option
    ; signature : Blob.t option
    ; signing_algorithm : SigningAlgorithmSpec.t option
    }

  let make ?key_id ?signature ?signing_algorithm () =
    { key_id; signature; signing_algorithm }

  let parse xml =
    Some
      { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; signature = Util.option_bind (Xml.member "Signature" xml) Blob.parse
      ; signing_algorithm =
          Util.option_bind (Xml.member "SigningAlgorithm" xml) SigningAlgorithmSpec.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.signing_algorithm (fun f ->
               Query.Pair ("SigningAlgorithm", SigningAlgorithmSpec.to_query f))
         ; Util.option_map v.signature (fun f ->
               Query.Pair ("Signature", Blob.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.signing_algorithm (fun f ->
               "signing_algorithm", SigningAlgorithmSpec.to_json f)
         ; Util.option_map v.signature (fun f -> "signature", Blob.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ])

  let of_json j =
    { key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; signature = Util.option_map (Json.lookup j "signature") Blob.of_json
    ; signing_algorithm =
        Util.option_map (Json.lookup j "signing_algorithm") SigningAlgorithmSpec.of_json
    }
end

module AlreadyExistsException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module UpdateAliasRequest = struct
  type t =
    { alias_name : String.t
    ; target_key_id : String.t
    }

  let make ~alias_name ~target_key_id () = { alias_name; target_key_id }

  let parse xml =
    Some
      { alias_name =
          Xml.required
            "AliasName"
            (Util.option_bind (Xml.member "AliasName" xml) String.parse)
      ; target_key_id =
          Xml.required
            "TargetKeyId"
            (Util.option_bind (Xml.member "TargetKeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TargetKeyId", String.to_query v.target_key_id))
         ; Some (Query.Pair ("AliasName", String.to_query v.alias_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("target_key_id", String.to_json v.target_key_id)
         ; Some ("alias_name", String.to_json v.alias_name)
         ])

  let of_json j =
    { alias_name = String.of_json (Util.of_option_exn (Json.lookup j "alias_name"))
    ; target_key_id = String.of_json (Util.of_option_exn (Json.lookup j "target_key_id"))
    }
end

module InvalidGrantTokenException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListKeyPoliciesResponse = struct
  type t =
    { policy_names : PolicyNameList.t
    ; next_marker : String.t option
    ; truncated : Boolean.t option
    }

  let make ?(policy_names = []) ?next_marker ?truncated () =
    { policy_names; next_marker; truncated }

  let parse xml =
    Some
      { policy_names =
          Util.of_option
            []
            (Util.option_bind (Xml.member "PolicyNames" xml) PolicyNameList.parse)
      ; next_marker = Util.option_bind (Xml.member "NextMarker" xml) String.parse
      ; truncated = Util.option_bind (Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f ->
               Query.Pair ("Truncated", Boolean.to_query f))
         ; Util.option_map v.next_marker (fun f ->
               Query.Pair ("NextMarker", String.to_query f))
         ; Some
             (Query.Pair ("PolicyNames.member", PolicyNameList.to_query v.policy_names))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f -> "truncated", Boolean.to_json f)
         ; Util.option_map v.next_marker (fun f -> "next_marker", String.to_json f)
         ; Some ("policy_names", PolicyNameList.to_json v.policy_names)
         ])

  let of_json j =
    { policy_names =
        PolicyNameList.of_json (Util.of_option_exn (Json.lookup j "policy_names"))
    ; next_marker = Util.option_map (Json.lookup j "next_marker") String.of_json
    ; truncated = Util.option_map (Json.lookup j "truncated") Boolean.of_json
    }
end

module VerifyResponse = struct
  type t =
    { key_id : String.t option
    ; signature_valid : Boolean.t option
    ; signing_algorithm : SigningAlgorithmSpec.t option
    }

  let make ?key_id ?signature_valid ?signing_algorithm () =
    { key_id; signature_valid; signing_algorithm }

  let parse xml =
    Some
      { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; signature_valid = Util.option_bind (Xml.member "SignatureValid" xml) Boolean.parse
      ; signing_algorithm =
          Util.option_bind (Xml.member "SigningAlgorithm" xml) SigningAlgorithmSpec.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.signing_algorithm (fun f ->
               Query.Pair ("SigningAlgorithm", SigningAlgorithmSpec.to_query f))
         ; Util.option_map v.signature_valid (fun f ->
               Query.Pair ("SignatureValid", Boolean.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.signing_algorithm (fun f ->
               "signing_algorithm", SigningAlgorithmSpec.to_json f)
         ; Util.option_map v.signature_valid (fun f ->
               "signature_valid", Boolean.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ])

  let of_json j =
    { key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; signature_valid = Util.option_map (Json.lookup j "signature_valid") Boolean.of_json
    ; signing_algorithm =
        Util.option_map (Json.lookup j "signing_algorithm") SigningAlgorithmSpec.of_json
    }
end

module ListGrantsResponse = struct
  type t =
    { grants : GrantList.t
    ; next_marker : String.t option
    ; truncated : Boolean.t option
    }

  let make ?(grants = []) ?next_marker ?truncated () = { grants; next_marker; truncated }

  let parse xml =
    Some
      { grants =
          Util.of_option [] (Util.option_bind (Xml.member "Grants" xml) GrantList.parse)
      ; next_marker = Util.option_bind (Xml.member "NextMarker" xml) String.parse
      ; truncated = Util.option_bind (Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f ->
               Query.Pair ("Truncated", Boolean.to_query f))
         ; Util.option_map v.next_marker (fun f ->
               Query.Pair ("NextMarker", String.to_query f))
         ; Some (Query.Pair ("Grants.member", GrantList.to_query v.grants))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f -> "truncated", Boolean.to_json f)
         ; Util.option_map v.next_marker (fun f -> "next_marker", String.to_json f)
         ; Some ("grants", GrantList.to_json v.grants)
         ])

  let of_json j =
    { grants = GrantList.of_json (Util.of_option_exn (Json.lookup j "grants"))
    ; next_marker = Util.option_map (Json.lookup j "next_marker") String.of_json
    ; truncated = Util.option_map (Json.lookup j "truncated") Boolean.of_json
    }
end

module ListAliasesRequest = struct
  type t =
    { key_id : String.t option
    ; limit : Integer.t option
    ; marker : String.t option
    }

  let make ?key_id ?limit ?marker () = { key_id; limit; marker }

  let parse xml =
    Some
      { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; limit = Util.option_bind (Xml.member "Limit" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.limit (fun f -> Query.Pair ("Limit", Integer.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.limit (fun f -> "limit", Integer.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ])

  let of_json j =
    { key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; limit = Util.option_map (Json.lookup j "limit") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module SignRequest = struct
  type t =
    { key_id : String.t
    ; message : Blob.t
    ; message_type : MessageType.t option
    ; grant_tokens : GrantTokenList.t
    ; signing_algorithm : SigningAlgorithmSpec.t
    }

  let make ~key_id ~message ?message_type ?(grant_tokens = []) ~signing_algorithm () =
    { key_id; message; message_type; grant_tokens; signing_algorithm }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; message =
          Xml.required "Message" (Util.option_bind (Xml.member "Message" xml) Blob.parse)
      ; message_type = Util.option_bind (Xml.member "MessageType" xml) MessageType.parse
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      ; signing_algorithm =
          Xml.required
            "SigningAlgorithm"
            (Util.option_bind
               (Xml.member "SigningAlgorithm" xml)
               SigningAlgorithmSpec.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("SigningAlgorithm", SigningAlgorithmSpec.to_query v.signing_algorithm))
         ; Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Util.option_map v.message_type (fun f ->
               Query.Pair ("MessageType", MessageType.to_query f))
         ; Some (Query.Pair ("Message", Blob.to_query v.message))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("signing_algorithm", SigningAlgorithmSpec.to_json v.signing_algorithm)
         ; Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Util.option_map v.message_type (fun f -> "message_type", MessageType.to_json f)
         ; Some ("message", Blob.to_json v.message)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; message = Blob.of_json (Util.of_option_exn (Json.lookup j "message"))
    ; message_type = Util.option_map (Json.lookup j "message_type") MessageType.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    ; signing_algorithm =
        SigningAlgorithmSpec.of_json
          (Util.of_option_exn (Json.lookup j "signing_algorithm"))
    }
end

module ListKeyPoliciesRequest = struct
  type t =
    { key_id : String.t
    ; limit : Integer.t option
    ; marker : String.t option
    }

  let make ~key_id ?limit ?marker () = { key_id; limit; marker }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; limit = Util.option_bind (Xml.member "Limit" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.limit (fun f -> Query.Pair ("Limit", Integer.to_query f))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.limit (fun f -> "limit", Integer.to_json f)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; limit = Util.option_map (Json.lookup j "limit") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module CloudHsmClusterNotFoundException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GenerateDataKeyPairWithoutPlaintextRequest = struct
  type t =
    { encryption_context : EncryptionContextType.t option
    ; key_id : String.t
    ; key_pair_spec : DataKeyPairSpec.t
    ; grant_tokens : GrantTokenList.t
    }

  let make ?encryption_context ~key_id ~key_pair_spec ?(grant_tokens = []) () =
    { encryption_context; key_id; key_pair_spec; grant_tokens }

  let parse xml =
    Some
      { encryption_context =
          Util.option_bind
            (Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; key_pair_spec =
          Xml.required
            "KeyPairSpec"
            (Util.option_bind (Xml.member "KeyPairSpec" xml) DataKeyPairSpec.parse)
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some (Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query v.key_pair_spec))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ; Util.option_map v.encryption_context (fun f ->
               Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("key_pair_spec", DataKeyPairSpec.to_json v.key_pair_spec)
         ; Some ("key_id", String.to_json v.key_id)
         ; Util.option_map v.encryption_context (fun f ->
               "encryption_context", EncryptionContextType.to_json f)
         ])

  let of_json j =
    { encryption_context =
        Util.option_map (Json.lookup j "encryption_context") EncryptionContextType.of_json
    ; key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; key_pair_spec =
        DataKeyPairSpec.of_json (Util.of_option_exn (Json.lookup j "key_pair_spec"))
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    }
end

module NotFoundException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module IncorrectKeyMaterialException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module CreateGrantResponse = struct
  type t =
    { grant_token : String.t option
    ; grant_id : String.t option
    }

  let make ?grant_token ?grant_id () = { grant_token; grant_id }

  let parse xml =
    Some
      { grant_token = Util.option_bind (Xml.member "GrantToken" xml) String.parse
      ; grant_id = Util.option_bind (Xml.member "GrantId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.grant_id (fun f -> Query.Pair ("GrantId", String.to_query f))
         ; Util.option_map v.grant_token (fun f ->
               Query.Pair ("GrantToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.grant_id (fun f -> "grant_id", String.to_json f)
         ; Util.option_map v.grant_token (fun f -> "grant_token", String.to_json f)
         ])

  let of_json j =
    { grant_token = Util.option_map (Json.lookup j "grant_token") String.of_json
    ; grant_id = Util.option_map (Json.lookup j "grant_id") String.of_json
    }
end

module DeleteAliasRequest = struct
  type t = { alias_name : String.t }

  let make ~alias_name () = { alias_name }

  let parse xml =
    Some
      { alias_name =
          Xml.required
            "AliasName"
            (Util.option_bind (Xml.member "AliasName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("AliasName", String.to_query v.alias_name)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("alias_name", String.to_json v.alias_name) ])

  let of_json j =
    { alias_name = String.of_json (Util.of_option_exn (Json.lookup j "alias_name")) }
end

module PutKeyPolicyRequest = struct
  type t =
    { key_id : String.t
    ; policy_name : String.t
    ; policy : String.t
    ; bypass_policy_lockout_safety_check : Boolean.t option
    }

  let make ~key_id ~policy_name ~policy ?bypass_policy_lockout_safety_check () =
    { key_id; policy_name; policy; bypass_policy_lockout_safety_check }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; policy_name =
          Xml.required
            "PolicyName"
            (Util.option_bind (Xml.member "PolicyName" xml) String.parse)
      ; policy =
          Xml.required "Policy" (Util.option_bind (Xml.member "Policy" xml) String.parse)
      ; bypass_policy_lockout_safety_check =
          Util.option_bind (Xml.member "BypassPolicyLockoutSafetyCheck" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               Query.Pair ("BypassPolicyLockoutSafetyCheck", Boolean.to_query f))
         ; Some (Query.Pair ("Policy", String.to_query v.policy))
         ; Some (Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               "bypass_policy_lockout_safety_check", Boolean.to_json f)
         ; Some ("policy", String.to_json v.policy)
         ; Some ("policy_name", String.to_json v.policy_name)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; policy_name = String.of_json (Util.of_option_exn (Json.lookup j "policy_name"))
    ; policy = String.of_json (Util.of_option_exn (Json.lookup j "policy"))
    ; bypass_policy_lockout_safety_check =
        Util.option_map
          (Json.lookup j "bypass_policy_lockout_safety_check")
          Boolean.of_json
    }
end

module DecryptRequest = struct
  type t =
    { ciphertext_blob : Blob.t
    ; encryption_context : EncryptionContextType.t option
    ; grant_tokens : GrantTokenList.t
    ; key_id : String.t option
    ; encryption_algorithm : EncryptionAlgorithmSpec.t option
    }

  let make
      ~ciphertext_blob
      ?encryption_context
      ?(grant_tokens = [])
      ?key_id
      ?encryption_algorithm
      () =
    { ciphertext_blob; encryption_context; grant_tokens; key_id; encryption_algorithm }

  let parse xml =
    Some
      { ciphertext_blob =
          Xml.required
            "CiphertextBlob"
            (Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse)
      ; encryption_context =
          Util.option_bind
            (Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      ; key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; encryption_algorithm =
          Util.option_bind
            (Xml.member "EncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.encryption_algorithm (fun f ->
               Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ; Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Util.option_map v.encryption_context (fun f ->
               Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ; Some (Query.Pair ("CiphertextBlob", Blob.to_query v.ciphertext_blob))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.encryption_algorithm (fun f ->
               "encryption_algorithm", EncryptionAlgorithmSpec.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ; Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Util.option_map v.encryption_context (fun f ->
               "encryption_context", EncryptionContextType.to_json f)
         ; Some ("ciphertext_blob", Blob.to_json v.ciphertext_blob)
         ])

  let of_json j =
    { ciphertext_blob =
        Blob.of_json (Util.of_option_exn (Json.lookup j "ciphertext_blob"))
    ; encryption_context =
        Util.option_map (Json.lookup j "encryption_context") EncryptionContextType.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    ; key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; encryption_algorithm =
        Util.option_map
          (Json.lookup j "encryption_algorithm")
          EncryptionAlgorithmSpec.of_json
    }
end

module DisabledException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetKeyPolicyRequest = struct
  type t =
    { key_id : String.t
    ; policy_name : String.t
    }

  let make ~key_id ~policy_name () = { key_id; policy_name }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; policy_name =
          Xml.required
            "PolicyName"
            (Util.option_bind (Xml.member "PolicyName" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("policy_name", String.to_json v.policy_name)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; policy_name = String.of_json (Util.of_option_exn (Json.lookup j "policy_name"))
    }
end

module CloudHsmClusterInvalidConfigurationException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListKeysRequest = struct
  type t =
    { limit : Integer.t option
    ; marker : String.t option
    }

  let make ?limit ?marker () = { limit; marker }

  let parse xml =
    Some
      { limit = Util.option_bind (Xml.member "Limit" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.limit (fun f -> Query.Pair ("Limit", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.limit (fun f -> "limit", Integer.to_json f)
         ])

  let of_json j =
    { limit = Util.option_map (Json.lookup j "limit") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module UpdatePrimaryRegionRequest = struct
  type t =
    { key_id : String.t
    ; primary_region : String.t
    }

  let make ~key_id ~primary_region () = { key_id; primary_region }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; primary_region =
          Xml.required
            "PrimaryRegion"
            (Util.option_bind (Xml.member "PrimaryRegion" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("PrimaryRegion", String.to_query v.primary_region))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("primary_region", String.to_json v.primary_region)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; primary_region =
        String.of_json (Util.of_option_exn (Json.lookup j "primary_region"))
    }
end

module GenerateDataKeyResponse = struct
  type t =
    { ciphertext_blob : Blob.t
    ; plaintext : Blob.t
    ; key_id : String.t
    }

  let make ~ciphertext_blob ~plaintext ~key_id () = { ciphertext_blob; plaintext; key_id }

  let parse xml =
    Some
      { ciphertext_blob =
          Xml.required
            "CiphertextBlob"
            (Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse)
      ; plaintext =
          Xml.required
            "Plaintext"
            (Util.option_bind (Xml.member "Plaintext" xml) Blob.parse)
      ; key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ; Some (Query.Pair ("Plaintext", Blob.to_query v.plaintext))
         ; Some (Query.Pair ("CiphertextBlob", Blob.to_query v.ciphertext_blob))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("key_id", String.to_json v.key_id)
         ; Some ("plaintext", Blob.to_json v.plaintext)
         ; Some ("ciphertext_blob", Blob.to_json v.ciphertext_blob)
         ])

  let of_json j =
    { ciphertext_blob =
        Blob.of_json (Util.of_option_exn (Json.lookup j "ciphertext_blob"))
    ; plaintext = Blob.of_json (Util.of_option_exn (Json.lookup j "plaintext"))
    ; key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    }
end

module InvalidKeyUsageException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module CreateGrantRequest = struct
  type t =
    { key_id : String.t
    ; grantee_principal : String.t
    ; retiring_principal : String.t option
    ; operations : GrantOperationList.t
    ; constraints : GrantConstraints.t option
    ; grant_tokens : GrantTokenList.t
    ; name : String.t option
    }

  let make
      ~key_id
      ~grantee_principal
      ?retiring_principal
      ~operations
      ?constraints
      ?(grant_tokens = [])
      ?name
      () =
    { key_id
    ; grantee_principal
    ; retiring_principal
    ; operations
    ; constraints
    ; grant_tokens
    ; name
    }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; grantee_principal =
          Xml.required
            "GranteePrincipal"
            (Util.option_bind (Xml.member "GranteePrincipal" xml) String.parse)
      ; retiring_principal =
          Util.option_bind (Xml.member "RetiringPrincipal" xml) String.parse
      ; operations =
          Xml.required
            "Operations"
            (Util.option_bind (Xml.member "Operations" xml) GrantOperationList.parse)
      ; constraints =
          Util.option_bind (Xml.member "Constraints" xml) GrantConstraints.parse
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      ; name = Util.option_bind (Xml.member "Name" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ; Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Util.option_map v.constraints (fun f ->
               Query.Pair ("Constraints", GrantConstraints.to_query f))
         ; Some
             (Query.Pair ("Operations.member", GrantOperationList.to_query v.operations))
         ; Util.option_map v.retiring_principal (fun f ->
               Query.Pair ("RetiringPrincipal", String.to_query f))
         ; Some (Query.Pair ("GranteePrincipal", String.to_query v.grantee_principal))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.name (fun f -> "name", String.to_json f)
         ; Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Util.option_map v.constraints (fun f ->
               "constraints", GrantConstraints.to_json f)
         ; Some ("operations", GrantOperationList.to_json v.operations)
         ; Util.option_map v.retiring_principal (fun f ->
               "retiring_principal", String.to_json f)
         ; Some ("grantee_principal", String.to_json v.grantee_principal)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; grantee_principal =
        String.of_json (Util.of_option_exn (Json.lookup j "grantee_principal"))
    ; retiring_principal =
        Util.option_map (Json.lookup j "retiring_principal") String.of_json
    ; operations =
        GrantOperationList.of_json (Util.of_option_exn (Json.lookup j "operations"))
    ; constraints = Util.option_map (Json.lookup j "constraints") GrantConstraints.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    ; name = Util.option_map (Json.lookup j "name") String.of_json
    }
end

module GenerateDataKeyWithoutPlaintextRequest = struct
  type t =
    { key_id : String.t
    ; encryption_context : EncryptionContextType.t option
    ; key_spec : DataKeySpec.t option
    ; number_of_bytes : Integer.t option
    ; grant_tokens : GrantTokenList.t
    }

  let make ~key_id ?encryption_context ?key_spec ?number_of_bytes ?(grant_tokens = []) ()
      =
    { key_id; encryption_context; key_spec; number_of_bytes; grant_tokens }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; encryption_context =
          Util.option_bind
            (Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; key_spec = Util.option_bind (Xml.member "KeySpec" xml) DataKeySpec.parse
      ; number_of_bytes = Util.option_bind (Xml.member "NumberOfBytes" xml) Integer.parse
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Util.option_map v.number_of_bytes (fun f ->
               Query.Pair ("NumberOfBytes", Integer.to_query f))
         ; Util.option_map v.key_spec (fun f ->
               Query.Pair ("KeySpec", DataKeySpec.to_query f))
         ; Util.option_map v.encryption_context (fun f ->
               Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Util.option_map v.number_of_bytes (fun f ->
               "number_of_bytes", Integer.to_json f)
         ; Util.option_map v.key_spec (fun f -> "key_spec", DataKeySpec.to_json f)
         ; Util.option_map v.encryption_context (fun f ->
               "encryption_context", EncryptionContextType.to_json f)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; encryption_context =
        Util.option_map (Json.lookup j "encryption_context") EncryptionContextType.of_json
    ; key_spec = Util.option_map (Json.lookup j "key_spec") DataKeySpec.of_json
    ; number_of_bytes = Util.option_map (Json.lookup j "number_of_bytes") Integer.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    }
end

module UnsupportedOperationException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module CreateAliasRequest = struct
  type t =
    { alias_name : String.t
    ; target_key_id : String.t
    }

  let make ~alias_name ~target_key_id () = { alias_name; target_key_id }

  let parse xml =
    Some
      { alias_name =
          Xml.required
            "AliasName"
            (Util.option_bind (Xml.member "AliasName" xml) String.parse)
      ; target_key_id =
          Xml.required
            "TargetKeyId"
            (Util.option_bind (Xml.member "TargetKeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TargetKeyId", String.to_query v.target_key_id))
         ; Some (Query.Pair ("AliasName", String.to_query v.alias_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("target_key_id", String.to_json v.target_key_id)
         ; Some ("alias_name", String.to_json v.alias_name)
         ])

  let of_json j =
    { alias_name = String.of_json (Util.of_option_exn (Json.lookup j "alias_name"))
    ; target_key_id = String.of_json (Util.of_option_exn (Json.lookup j "target_key_id"))
    }
end

module GenerateDataKeyPairResponse = struct
  type t =
    { private_key_ciphertext_blob : Blob.t option
    ; private_key_plaintext : Blob.t option
    ; public_key : Blob.t option
    ; key_id : String.t option
    ; key_pair_spec : DataKeyPairSpec.t option
    }

  let make
      ?private_key_ciphertext_blob
      ?private_key_plaintext
      ?public_key
      ?key_id
      ?key_pair_spec
      () =
    { private_key_ciphertext_blob
    ; private_key_plaintext
    ; public_key
    ; key_id
    ; key_pair_spec
    }

  let parse xml =
    Some
      { private_key_ciphertext_blob =
          Util.option_bind (Xml.member "PrivateKeyCiphertextBlob" xml) Blob.parse
      ; private_key_plaintext =
          Util.option_bind (Xml.member "PrivateKeyPlaintext" xml) Blob.parse
      ; public_key = Util.option_bind (Xml.member "PublicKey" xml) Blob.parse
      ; key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; key_pair_spec =
          Util.option_bind (Xml.member "KeyPairSpec" xml) DataKeyPairSpec.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.key_pair_spec (fun f ->
               Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ; Util.option_map v.public_key (fun f ->
               Query.Pair ("PublicKey", Blob.to_query f))
         ; Util.option_map v.private_key_plaintext (fun f ->
               Query.Pair ("PrivateKeyPlaintext", Blob.to_query f))
         ; Util.option_map v.private_key_ciphertext_blob (fun f ->
               Query.Pair ("PrivateKeyCiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.key_pair_spec (fun f ->
               "key_pair_spec", DataKeyPairSpec.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ; Util.option_map v.public_key (fun f -> "public_key", Blob.to_json f)
         ; Util.option_map v.private_key_plaintext (fun f ->
               "private_key_plaintext", Blob.to_json f)
         ; Util.option_map v.private_key_ciphertext_blob (fun f ->
               "private_key_ciphertext_blob", Blob.to_json f)
         ])

  let of_json j =
    { private_key_ciphertext_blob =
        Util.option_map (Json.lookup j "private_key_ciphertext_blob") Blob.of_json
    ; private_key_plaintext =
        Util.option_map (Json.lookup j "private_key_plaintext") Blob.of_json
    ; public_key = Util.option_map (Json.lookup j "public_key") Blob.of_json
    ; key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; key_pair_spec =
        Util.option_map (Json.lookup j "key_pair_spec") DataKeyPairSpec.of_json
    }
end

module ListGrantsRequest = struct
  type t =
    { limit : Integer.t option
    ; marker : String.t option
    ; key_id : String.t
    ; grant_id : String.t option
    ; grantee_principal : String.t option
    }

  let make ?limit ?marker ~key_id ?grant_id ?grantee_principal () =
    { limit; marker; key_id; grant_id; grantee_principal }

  let parse xml =
    Some
      { limit = Util.option_bind (Xml.member "Limit" xml) Integer.parse
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      ; key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; grant_id = Util.option_bind (Xml.member "GrantId" xml) String.parse
      ; grantee_principal =
          Util.option_bind (Xml.member "GranteePrincipal" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.grantee_principal (fun f ->
               Query.Pair ("GranteePrincipal", String.to_query f))
         ; Util.option_map v.grant_id (fun f -> Query.Pair ("GrantId", String.to_query f))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ; Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Util.option_map v.limit (fun f -> Query.Pair ("Limit", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.grantee_principal (fun f ->
               "grantee_principal", String.to_json f)
         ; Util.option_map v.grant_id (fun f -> "grant_id", String.to_json f)
         ; Some ("key_id", String.to_json v.key_id)
         ; Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Util.option_map v.limit (fun f -> "limit", Integer.to_json f)
         ])

  let of_json j =
    { limit = Util.option_map (Json.lookup j "limit") Integer.of_json
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    ; key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; grant_id = Util.option_map (Json.lookup j "grant_id") String.of_json
    ; grantee_principal =
        Util.option_map (Json.lookup j "grantee_principal") String.of_json
    }
end

module RevokeGrantRequest = struct
  type t =
    { key_id : String.t
    ; grant_id : String.t
    }

  let make ~key_id ~grant_id () = { key_id; grant_id }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; grant_id =
          Xml.required
            "GrantId"
            (Util.option_bind (Xml.member "GrantId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("GrantId", String.to_query v.grant_id))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("grant_id", String.to_json v.grant_id)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; grant_id = String.of_json (Util.of_option_exn (Json.lookup j "grant_id"))
    }
end

module GenerateDataKeyWithoutPlaintextResponse = struct
  type t =
    { ciphertext_blob : Blob.t option
    ; key_id : String.t option
    }

  let make ?ciphertext_blob ?key_id () = { ciphertext_blob; key_id }

  let parse xml =
    Some
      { ciphertext_blob = Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse
      ; key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ; Util.option_map v.ciphertext_blob (fun f ->
               Query.Pair ("CiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ; Util.option_map v.ciphertext_blob (fun f -> "ciphertext_blob", Blob.to_json f)
         ])

  let of_json j =
    { ciphertext_blob = Util.option_map (Json.lookup j "ciphertext_blob") Blob.of_json
    ; key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    }
end

module LimitExceededException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ReplicateKeyRequest = struct
  type t =
    { key_id : String.t
    ; replica_region : String.t
    ; policy : String.t option
    ; bypass_policy_lockout_safety_check : Boolean.t option
    ; description : String.t option
    ; tags : TagList.t
    }

  let make
      ~key_id
      ~replica_region
      ?policy
      ?bypass_policy_lockout_safety_check
      ?description
      ?(tags = [])
      () =
    { key_id
    ; replica_region
    ; policy
    ; bypass_policy_lockout_safety_check
    ; description
    ; tags
    }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; replica_region =
          Xml.required
            "ReplicaRegion"
            (Util.option_bind (Xml.member "ReplicaRegion" xml) String.parse)
      ; policy = Util.option_bind (Xml.member "Policy" xml) String.parse
      ; bypass_policy_lockout_safety_check =
          Util.option_bind (Xml.member "BypassPolicyLockoutSafetyCheck" xml) Boolean.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               Query.Pair ("BypassPolicyLockoutSafetyCheck", Boolean.to_query f))
         ; Util.option_map v.policy (fun f -> Query.Pair ("Policy", String.to_query f))
         ; Some (Query.Pair ("ReplicaRegion", String.to_query v.replica_region))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               "bypass_policy_lockout_safety_check", Boolean.to_json f)
         ; Util.option_map v.policy (fun f -> "policy", String.to_json f)
         ; Some ("replica_region", String.to_json v.replica_region)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; replica_region =
        String.of_json (Util.of_option_exn (Json.lookup j "replica_region"))
    ; policy = Util.option_map (Json.lookup j "policy") String.of_json
    ; bypass_policy_lockout_safety_check =
        Util.option_map
          (Json.lookup j "bypass_policy_lockout_safety_check")
          Boolean.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module GetKeyRotationStatusResponse = struct
  type t = { key_rotation_enabled : Boolean.t option }

  let make ?key_rotation_enabled () = { key_rotation_enabled }

  let parse xml =
    Some
      { key_rotation_enabled =
          Util.option_bind (Xml.member "KeyRotationEnabled" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.key_rotation_enabled (fun f ->
               Query.Pair ("KeyRotationEnabled", Boolean.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.key_rotation_enabled (fun f ->
               "key_rotation_enabled", Boolean.to_json f)
         ])

  let of_json j =
    { key_rotation_enabled =
        Util.option_map (Json.lookup j "key_rotation_enabled") Boolean.of_json
    }
end

module ConnectCustomKeyStoreResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DisconnectCustomKeyStoreRequest = struct
  type t = { custom_key_store_id : String.t }

  let make ~custom_key_store_id () = { custom_key_store_id }

  let parse xml =
    Some
      { custom_key_store_id =
          Xml.required
            "CustomKeyStoreId"
            (Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("custom_key_store_id", String.to_json v.custom_key_store_id) ])

  let of_json j =
    { custom_key_store_id =
        String.of_json (Util.of_option_exn (Json.lookup j "custom_key_store_id"))
    }
end

module ListAliasesResponse = struct
  type t =
    { aliases : AliasList.t
    ; next_marker : String.t option
    ; truncated : Boolean.t option
    }

  let make ?(aliases = []) ?next_marker ?truncated () =
    { aliases; next_marker; truncated }

  let parse xml =
    Some
      { aliases =
          Util.of_option [] (Util.option_bind (Xml.member "Aliases" xml) AliasList.parse)
      ; next_marker = Util.option_bind (Xml.member "NextMarker" xml) String.parse
      ; truncated = Util.option_bind (Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f ->
               Query.Pair ("Truncated", Boolean.to_query f))
         ; Util.option_map v.next_marker (fun f ->
               Query.Pair ("NextMarker", String.to_query f))
         ; Some (Query.Pair ("Aliases.member", AliasList.to_query v.aliases))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.truncated (fun f -> "truncated", Boolean.to_json f)
         ; Util.option_map v.next_marker (fun f -> "next_marker", String.to_json f)
         ; Some ("aliases", AliasList.to_json v.aliases)
         ])

  let of_json j =
    { aliases = AliasList.of_json (Util.of_option_exn (Json.lookup j "aliases"))
    ; next_marker = Util.option_map (Json.lookup j "next_marker") String.of_json
    ; truncated = Util.option_map (Json.lookup j "truncated") Boolean.of_json
    }
end

module UpdateKeyDescriptionRequest = struct
  type t =
    { key_id : String.t
    ; description : String.t
    }

  let make ~key_id ~description () = { key_id; description }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; description =
          Xml.required
            "Description"
            (Util.option_bind (Xml.member "Description" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Description", String.to_query v.description))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("description", String.to_json v.description)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; description = String.of_json (Util.of_option_exn (Json.lookup j "description"))
    }
end

module CreateKeyRequest = struct
  type t =
    { policy : String.t option
    ; description : String.t option
    ; key_usage : KeyUsageType.t option
    ; customer_master_key_spec : CustomerMasterKeySpec.t option
    ; key_spec : KeySpec.t option
    ; origin : OriginType.t option
    ; custom_key_store_id : String.t option
    ; bypass_policy_lockout_safety_check : Boolean.t option
    ; tags : TagList.t
    ; multi_region : Boolean.t option
    }

  let make
      ?policy
      ?description
      ?key_usage
      ?customer_master_key_spec
      ?key_spec
      ?origin
      ?custom_key_store_id
      ?bypass_policy_lockout_safety_check
      ?(tags = [])
      ?multi_region
      () =
    { policy
    ; description
    ; key_usage
    ; customer_master_key_spec
    ; key_spec
    ; origin
    ; custom_key_store_id
    ; bypass_policy_lockout_safety_check
    ; tags
    ; multi_region
    }

  let parse xml =
    Some
      { policy = Util.option_bind (Xml.member "Policy" xml) String.parse
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; key_usage = Util.option_bind (Xml.member "KeyUsage" xml) KeyUsageType.parse
      ; customer_master_key_spec =
          Util.option_bind
            (Xml.member "CustomerMasterKeySpec" xml)
            CustomerMasterKeySpec.parse
      ; key_spec = Util.option_bind (Xml.member "KeySpec" xml) KeySpec.parse
      ; origin = Util.option_bind (Xml.member "Origin" xml) OriginType.parse
      ; custom_key_store_id =
          Util.option_bind (Xml.member "CustomKeyStoreId" xml) String.parse
      ; bypass_policy_lockout_safety_check =
          Util.option_bind (Xml.member "BypassPolicyLockoutSafetyCheck" xml) Boolean.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; multi_region = Util.option_bind (Xml.member "MultiRegion" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.multi_region (fun f ->
               Query.Pair ("MultiRegion", Boolean.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               Query.Pair ("BypassPolicyLockoutSafetyCheck", Boolean.to_query f))
         ; Util.option_map v.custom_key_store_id (fun f ->
               Query.Pair ("CustomKeyStoreId", String.to_query f))
         ; Util.option_map v.origin (fun f ->
               Query.Pair ("Origin", OriginType.to_query f))
         ; Util.option_map v.key_spec (fun f ->
               Query.Pair ("KeySpec", KeySpec.to_query f))
         ; Util.option_map v.customer_master_key_spec (fun f ->
               Query.Pair ("CustomerMasterKeySpec", CustomerMasterKeySpec.to_query f))
         ; Util.option_map v.key_usage (fun f ->
               Query.Pair ("KeyUsage", KeyUsageType.to_query f))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Util.option_map v.policy (fun f -> Query.Pair ("Policy", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.multi_region (fun f -> "multi_region", Boolean.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               "bypass_policy_lockout_safety_check", Boolean.to_json f)
         ; Util.option_map v.custom_key_store_id (fun f ->
               "custom_key_store_id", String.to_json f)
         ; Util.option_map v.origin (fun f -> "origin", OriginType.to_json f)
         ; Util.option_map v.key_spec (fun f -> "key_spec", KeySpec.to_json f)
         ; Util.option_map v.customer_master_key_spec (fun f ->
               "customer_master_key_spec", CustomerMasterKeySpec.to_json f)
         ; Util.option_map v.key_usage (fun f -> "key_usage", KeyUsageType.to_json f)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Util.option_map v.policy (fun f -> "policy", String.to_json f)
         ])

  let of_json j =
    { policy = Util.option_map (Json.lookup j "policy") String.of_json
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; key_usage = Util.option_map (Json.lookup j "key_usage") KeyUsageType.of_json
    ; customer_master_key_spec =
        Util.option_map
          (Json.lookup j "customer_master_key_spec")
          CustomerMasterKeySpec.of_json
    ; key_spec = Util.option_map (Json.lookup j "key_spec") KeySpec.of_json
    ; origin = Util.option_map (Json.lookup j "origin") OriginType.of_json
    ; custom_key_store_id =
        Util.option_map (Json.lookup j "custom_key_store_id") String.of_json
    ; bypass_policy_lockout_safety_check =
        Util.option_map
          (Json.lookup j "bypass_policy_lockout_safety_check")
          Boolean.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; multi_region = Util.option_map (Json.lookup j "multi_region") Boolean.of_json
    }
end

module GenerateDataKeyPairWithoutPlaintextResponse = struct
  type t =
    { private_key_ciphertext_blob : Blob.t option
    ; public_key : Blob.t option
    ; key_id : String.t option
    ; key_pair_spec : DataKeyPairSpec.t option
    }

  let make ?private_key_ciphertext_blob ?public_key ?key_id ?key_pair_spec () =
    { private_key_ciphertext_blob; public_key; key_id; key_pair_spec }

  let parse xml =
    Some
      { private_key_ciphertext_blob =
          Util.option_bind (Xml.member "PrivateKeyCiphertextBlob" xml) Blob.parse
      ; public_key = Util.option_bind (Xml.member "PublicKey" xml) Blob.parse
      ; key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; key_pair_spec =
          Util.option_bind (Xml.member "KeyPairSpec" xml) DataKeyPairSpec.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.key_pair_spec (fun f ->
               Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ; Util.option_map v.public_key (fun f ->
               Query.Pair ("PublicKey", Blob.to_query f))
         ; Util.option_map v.private_key_ciphertext_blob (fun f ->
               Query.Pair ("PrivateKeyCiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.key_pair_spec (fun f ->
               "key_pair_spec", DataKeyPairSpec.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ; Util.option_map v.public_key (fun f -> "public_key", Blob.to_json f)
         ; Util.option_map v.private_key_ciphertext_blob (fun f ->
               "private_key_ciphertext_blob", Blob.to_json f)
         ])

  let of_json j =
    { private_key_ciphertext_blob =
        Util.option_map (Json.lookup j "private_key_ciphertext_blob") Blob.of_json
    ; public_key = Util.option_map (Json.lookup j "public_key") Blob.of_json
    ; key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; key_pair_spec =
        Util.option_map (Json.lookup j "key_pair_spec") DataKeyPairSpec.of_json
    }
end

module InvalidAliasNameException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module CreateCustomKeyStoreRequest = struct
  type t =
    { custom_key_store_name : String.t
    ; cloud_hsm_cluster_id : String.t
    ; trust_anchor_certificate : String.t
    ; key_store_password : String.t
    }

  let make
      ~custom_key_store_name
      ~cloud_hsm_cluster_id
      ~trust_anchor_certificate
      ~key_store_password
      () =
    { custom_key_store_name
    ; cloud_hsm_cluster_id
    ; trust_anchor_certificate
    ; key_store_password
    }

  let parse xml =
    Some
      { custom_key_store_name =
          Xml.required
            "CustomKeyStoreName"
            (Util.option_bind (Xml.member "CustomKeyStoreName" xml) String.parse)
      ; cloud_hsm_cluster_id =
          Xml.required
            "CloudHsmClusterId"
            (Util.option_bind (Xml.member "CloudHsmClusterId" xml) String.parse)
      ; trust_anchor_certificate =
          Xml.required
            "TrustAnchorCertificate"
            (Util.option_bind (Xml.member "TrustAnchorCertificate" xml) String.parse)
      ; key_store_password =
          Xml.required
            "KeyStorePassword"
            (Util.option_bind (Xml.member "KeyStorePassword" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("KeyStorePassword", String.to_query v.key_store_password))
         ; Some
             (Query.Pair
                ("TrustAnchorCertificate", String.to_query v.trust_anchor_certificate))
         ; Some (Query.Pair ("CloudHsmClusterId", String.to_query v.cloud_hsm_cluster_id))
         ; Some
             (Query.Pair ("CustomKeyStoreName", String.to_query v.custom_key_store_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("key_store_password", String.to_json v.key_store_password)
         ; Some ("trust_anchor_certificate", String.to_json v.trust_anchor_certificate)
         ; Some ("cloud_hsm_cluster_id", String.to_json v.cloud_hsm_cluster_id)
         ; Some ("custom_key_store_name", String.to_json v.custom_key_store_name)
         ])

  let of_json j =
    { custom_key_store_name =
        String.of_json (Util.of_option_exn (Json.lookup j "custom_key_store_name"))
    ; cloud_hsm_cluster_id =
        String.of_json (Util.of_option_exn (Json.lookup j "cloud_hsm_cluster_id"))
    ; trust_anchor_certificate =
        String.of_json (Util.of_option_exn (Json.lookup j "trust_anchor_certificate"))
    ; key_store_password =
        String.of_json (Util.of_option_exn (Json.lookup j "key_store_password"))
    }
end

module UntagResourceRequest = struct
  type t =
    { key_id : String.t
    ; tag_keys : TagKeyList.t
    }

  let make ~key_id ~tag_keys () = { key_id; tag_keys }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; tag_keys =
          Xml.required
            "TagKeys"
            (Util.option_bind (Xml.member "TagKeys" xml) TagKeyList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TagKeys.member", TagKeyList.to_query v.tag_keys))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tag_keys", TagKeyList.to_json v.tag_keys)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; tag_keys = TagKeyList.of_json (Util.of_option_exn (Json.lookup j "tag_keys"))
    }
end

module IncorrectTrustAnchorException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ScheduleKeyDeletionResponse = struct
  type t =
    { key_id : String.t option
    ; deletion_date : DateTime.t option
    ; key_state : KeyState.t option
    ; pending_window_in_days : Integer.t option
    }

  let make ?key_id ?deletion_date ?key_state ?pending_window_in_days () =
    { key_id; deletion_date; key_state; pending_window_in_days }

  let parse xml =
    Some
      { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; deletion_date = Util.option_bind (Xml.member "DeletionDate" xml) DateTime.parse
      ; key_state = Util.option_bind (Xml.member "KeyState" xml) KeyState.parse
      ; pending_window_in_days =
          Util.option_bind (Xml.member "PendingWindowInDays" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.pending_window_in_days (fun f ->
               Query.Pair ("PendingWindowInDays", Integer.to_query f))
         ; Util.option_map v.key_state (fun f ->
               Query.Pair ("KeyState", KeyState.to_query f))
         ; Util.option_map v.deletion_date (fun f ->
               Query.Pair ("DeletionDate", DateTime.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.pending_window_in_days (fun f ->
               "pending_window_in_days", Integer.to_json f)
         ; Util.option_map v.key_state (fun f -> "key_state", KeyState.to_json f)
         ; Util.option_map v.deletion_date (fun f -> "deletion_date", DateTime.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ])

  let of_json j =
    { key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; deletion_date = Util.option_map (Json.lookup j "deletion_date") DateTime.of_json
    ; key_state = Util.option_map (Json.lookup j "key_state") KeyState.of_json
    ; pending_window_in_days =
        Util.option_map (Json.lookup j "pending_window_in_days") Integer.of_json
    }
end

module GetParametersForImportResponse = struct
  type t =
    { key_id : String.t option
    ; import_token : Blob.t option
    ; public_key : Blob.t option
    ; parameters_valid_to : DateTime.t option
    }

  let make ?key_id ?import_token ?public_key ?parameters_valid_to () =
    { key_id; import_token; public_key; parameters_valid_to }

  let parse xml =
    Some
      { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; import_token = Util.option_bind (Xml.member "ImportToken" xml) Blob.parse
      ; public_key = Util.option_bind (Xml.member "PublicKey" xml) Blob.parse
      ; parameters_valid_to =
          Util.option_bind (Xml.member "ParametersValidTo" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.parameters_valid_to (fun f ->
               Query.Pair ("ParametersValidTo", DateTime.to_query f))
         ; Util.option_map v.public_key (fun f ->
               Query.Pair ("PublicKey", Blob.to_query f))
         ; Util.option_map v.import_token (fun f ->
               Query.Pair ("ImportToken", Blob.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.parameters_valid_to (fun f ->
               "parameters_valid_to", DateTime.to_json f)
         ; Util.option_map v.public_key (fun f -> "public_key", Blob.to_json f)
         ; Util.option_map v.import_token (fun f -> "import_token", Blob.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ])

  let of_json j =
    { key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; import_token = Util.option_map (Json.lookup j "import_token") Blob.of_json
    ; public_key = Util.option_map (Json.lookup j "public_key") Blob.of_json
    ; parameters_valid_to =
        Util.option_map (Json.lookup j "parameters_valid_to") DateTime.of_json
    }
end

module InvalidGrantIdException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module TagException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module DescribeKeyRequest = struct
  type t =
    { key_id : String.t
    ; grant_tokens : GrantTokenList.t
    }

  let make ~key_id ?(grant_tokens = []) () = { key_id; grant_tokens }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      ; grant_tokens =
          Util.of_option
            []
            (Util.option_bind (Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some (Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("grant_tokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("key_id", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id"))
    ; grant_tokens =
        GrantTokenList.of_json (Util.of_option_exn (Json.lookup j "grant_tokens"))
    }
end

module DisableKeyRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("key_id", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id")) }
end

module CancelKeyDeletionResponse = struct
  type t = { key_id : String.t option }

  let make ?key_id () = { key_id }

  let parse xml = Some { key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.key_id (fun f -> "key_id", String.to_json f) ])

  let of_json j = { key_id = Util.option_map (Json.lookup j "key_id") String.of_json }
end

module ReEncryptResponse = struct
  type t =
    { ciphertext_blob : Blob.t option
    ; source_key_id : String.t option
    ; key_id : String.t option
    ; source_encryption_algorithm : EncryptionAlgorithmSpec.t option
    ; destination_encryption_algorithm : EncryptionAlgorithmSpec.t option
    }

  let make
      ?ciphertext_blob
      ?source_key_id
      ?key_id
      ?source_encryption_algorithm
      ?destination_encryption_algorithm
      () =
    { ciphertext_blob
    ; source_key_id
    ; key_id
    ; source_encryption_algorithm
    ; destination_encryption_algorithm
    }

  let parse xml =
    Some
      { ciphertext_blob = Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse
      ; source_key_id = Util.option_bind (Xml.member "SourceKeyId" xml) String.parse
      ; key_id = Util.option_bind (Xml.member "KeyId" xml) String.parse
      ; source_encryption_algorithm =
          Util.option_bind
            (Xml.member "SourceEncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      ; destination_encryption_algorithm =
          Util.option_bind
            (Xml.member "DestinationEncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.destination_encryption_algorithm (fun f ->
               Query.Pair
                 ("DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Util.option_map v.source_encryption_algorithm (fun f ->
               Query.Pair ("SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Util.option_map v.key_id (fun f -> Query.Pair ("KeyId", String.to_query f))
         ; Util.option_map v.source_key_id (fun f ->
               Query.Pair ("SourceKeyId", String.to_query f))
         ; Util.option_map v.ciphertext_blob (fun f ->
               Query.Pair ("CiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.destination_encryption_algorithm (fun f ->
               "destination_encryption_algorithm", EncryptionAlgorithmSpec.to_json f)
         ; Util.option_map v.source_encryption_algorithm (fun f ->
               "source_encryption_algorithm", EncryptionAlgorithmSpec.to_json f)
         ; Util.option_map v.key_id (fun f -> "key_id", String.to_json f)
         ; Util.option_map v.source_key_id (fun f -> "source_key_id", String.to_json f)
         ; Util.option_map v.ciphertext_blob (fun f -> "ciphertext_blob", Blob.to_json f)
         ])

  let of_json j =
    { ciphertext_blob = Util.option_map (Json.lookup j "ciphertext_blob") Blob.of_json
    ; source_key_id = Util.option_map (Json.lookup j "source_key_id") String.of_json
    ; key_id = Util.option_map (Json.lookup j "key_id") String.of_json
    ; source_encryption_algorithm =
        Util.option_map
          (Json.lookup j "source_encryption_algorithm")
          EncryptionAlgorithmSpec.of_json
    ; destination_encryption_algorithm =
        Util.option_map
          (Json.lookup j "destination_encryption_algorithm")
          EncryptionAlgorithmSpec.of_json
    }
end

module DisableKeyRotationRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("key_id", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id")) }
end

module CreateKeyResponse = struct
  type t = { key_metadata : KeyMetadata.t option }

  let make ?key_metadata () = { key_metadata }

  let parse xml =
    Some
      { key_metadata = Util.option_bind (Xml.member "KeyMetadata" xml) KeyMetadata.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.key_metadata (fun f ->
               Query.Pair ("KeyMetadata", KeyMetadata.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.key_metadata (fun f -> "key_metadata", KeyMetadata.to_json f)
         ])

  let of_json j =
    { key_metadata = Util.option_map (Json.lookup j "key_metadata") KeyMetadata.of_json }
end

module ReplicateKeyResponse = struct
  type t =
    { replica_key_metadata : KeyMetadata.t option
    ; replica_policy : String.t option
    ; replica_tags : TagList.t
    }

  let make ?replica_key_metadata ?replica_policy ?(replica_tags = []) () =
    { replica_key_metadata; replica_policy; replica_tags }

  let parse xml =
    Some
      { replica_key_metadata =
          Util.option_bind (Xml.member "ReplicaKeyMetadata" xml) KeyMetadata.parse
      ; replica_policy = Util.option_bind (Xml.member "ReplicaPolicy" xml) String.parse
      ; replica_tags =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ReplicaTags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ReplicaTags.member", TagList.to_query v.replica_tags))
         ; Util.option_map v.replica_policy (fun f ->
               Query.Pair ("ReplicaPolicy", String.to_query f))
         ; Util.option_map v.replica_key_metadata (fun f ->
               Query.Pair ("ReplicaKeyMetadata", KeyMetadata.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("replica_tags", TagList.to_json v.replica_tags)
         ; Util.option_map v.replica_policy (fun f -> "replica_policy", String.to_json f)
         ; Util.option_map v.replica_key_metadata (fun f ->
               "replica_key_metadata", KeyMetadata.to_json f)
         ])

  let of_json j =
    { replica_key_metadata =
        Util.option_map (Json.lookup j "replica_key_metadata") KeyMetadata.of_json
    ; replica_policy = Util.option_map (Json.lookup j "replica_policy") String.of_json
    ; replica_tags = TagList.of_json (Util.of_option_exn (Json.lookup j "replica_tags"))
    }
end

module CancelKeyDeletionRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Xml.required "KeyId" (Util.option_bind (Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("key_id", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Util.of_option_exn (Json.lookup j "key_id")) }
end
