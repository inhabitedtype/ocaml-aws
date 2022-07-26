open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module CancelKeyDeletionRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("KeyId", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId")) }
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
          Aws.Xml.required
            "TagKey"
            (Aws.Util.option_bind (Aws.Xml.member "TagKey" xml) String.parse)
      ; tag_value =
          Aws.Xml.required
            "TagValue"
            (Aws.Util.option_bind (Aws.Xml.member "TagValue" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagValue", String.to_query v.tag_value))
         ; Some (Aws.Query.Pair ("TagKey", String.to_query v.tag_key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagValue", String.to_json v.tag_value)
         ; Some ("TagKey", String.to_json v.tag_key)
         ])

  let of_json j =
    { tag_key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKey"))
    ; tag_value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagValue"))
    }
end

module TagList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Aws.Json.to_list Tag.of_json j
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module SigningAlgorithmSpecList = struct
  type t = SigningAlgorithmSpec.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map SigningAlgorithmSpec.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list SigningAlgorithmSpec.to_query v

  let to_json v = `List (List.map SigningAlgorithmSpec.to_json v)

  let of_json j = Aws.Json.to_list SigningAlgorithmSpec.of_json j
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module MultiRegionKeyType = struct
  type t =
    | PRIMARY
    | REPLICA

  let str_to_t = [ "REPLICA", REPLICA; "PRIMARY", PRIMARY ]

  let t_to_str = [ REPLICA, "REPLICA"; PRIMARY, "PRIMARY" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module MultiRegionKey = struct
  type t =
    { arn : String.t option
    ; region : String.t option
    }

  let make ?arn ?region () = { arn; region }

  let parse xml =
    Some
      { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
      ; region = Aws.Util.option_bind (Aws.Xml.member "Region" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.region (fun f ->
               Aws.Query.Pair ("Region", String.to_query f))
         ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.region (fun f -> "Region", String.to_json f)
         ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
         ])

  let of_json j =
    { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
    ; region = Aws.Util.option_map (Aws.Json.lookup j "Region") String.of_json
    }
end

module MultiRegionKeyList = struct
  type t = MultiRegionKey.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map MultiRegionKey.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list MultiRegionKey.to_query v

  let to_json v = `List (List.map MultiRegionKey.to_json v)

  let of_json j = Aws.Json.to_list MultiRegionKey.of_json j
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
          Aws.Util.option_bind
            (Aws.Xml.member "MultiRegionKeyType" xml)
            MultiRegionKeyType.parse
      ; primary_key =
          Aws.Util.option_bind (Aws.Xml.member "PrimaryKey" xml) MultiRegionKey.parse
      ; replica_keys =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicaKeys" xml)
               MultiRegionKeyList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("ReplicaKeys.member", MultiRegionKeyList.to_query v.replica_keys))
         ; Aws.Util.option_map v.primary_key (fun f ->
               Aws.Query.Pair ("PrimaryKey", MultiRegionKey.to_query f))
         ; Aws.Util.option_map v.multi_region_key_type (fun f ->
               Aws.Query.Pair ("MultiRegionKeyType", MultiRegionKeyType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ReplicaKeys", MultiRegionKeyList.to_json v.replica_keys)
         ; Aws.Util.option_map v.primary_key (fun f ->
               "PrimaryKey", MultiRegionKey.to_json f)
         ; Aws.Util.option_map v.multi_region_key_type (fun f ->
               "MultiRegionKeyType", MultiRegionKeyType.to_json f)
         ])

  let of_json j =
    { multi_region_key_type =
        Aws.Util.option_map
          (Aws.Json.lookup j "MultiRegionKeyType")
          MultiRegionKeyType.of_json
    ; primary_key =
        Aws.Util.option_map (Aws.Json.lookup j "PrimaryKey") MultiRegionKey.of_json
    ; replica_keys =
        MultiRegionKeyList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaKeys"))
    }
end

module KeyUsageType = struct
  type t =
    | SIGN_VERIFY
    | ENCRYPT_DECRYPT

  let str_to_t = [ "ENCRYPT_DECRYPT", ENCRYPT_DECRYPT; "SIGN_VERIFY", SIGN_VERIFY ]

  let t_to_str = [ ENCRYPT_DECRYPT, "ENCRYPT_DECRYPT"; SIGN_VERIFY, "SIGN_VERIFY" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module KeyManagerType = struct
  type t =
    | AWS
    | CUSTOMER

  let str_to_t = [ "CUSTOMER", CUSTOMER; "AWS", AWS ]

  let t_to_str = [ CUSTOMER, "CUSTOMER"; AWS, "AWS" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module EncryptionAlgorithmSpecList = struct
  type t = EncryptionAlgorithmSpec.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map EncryptionAlgorithmSpec.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list EncryptionAlgorithmSpec.to_query v

  let to_json v = `List (List.map EncryptionAlgorithmSpec.to_json v)

  let of_json j = Aws.Json.to_list EncryptionAlgorithmSpec.of_json j
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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
      { a_w_s_account_id =
          Aws.Util.option_bind (Aws.Xml.member "AWSAccountId" xml) String.parse
      ; key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
      ; creation_date =
          Aws.Util.option_bind (Aws.Xml.member "CreationDate" xml) DateTime.parse
      ; enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; key_usage =
          Aws.Util.option_bind (Aws.Xml.member "KeyUsage" xml) KeyUsageType.parse
      ; key_state = Aws.Util.option_bind (Aws.Xml.member "KeyState" xml) KeyState.parse
      ; deletion_date =
          Aws.Util.option_bind (Aws.Xml.member "DeletionDate" xml) DateTime.parse
      ; valid_to = Aws.Util.option_bind (Aws.Xml.member "ValidTo" xml) DateTime.parse
      ; origin = Aws.Util.option_bind (Aws.Xml.member "Origin" xml) OriginType.parse
      ; custom_key_store_id =
          Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
      ; cloud_hsm_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CloudHsmClusterId" xml) String.parse
      ; expiration_model =
          Aws.Util.option_bind
            (Aws.Xml.member "ExpirationModel" xml)
            ExpirationModelType.parse
      ; key_manager =
          Aws.Util.option_bind (Aws.Xml.member "KeyManager" xml) KeyManagerType.parse
      ; customer_master_key_spec =
          Aws.Util.option_bind
            (Aws.Xml.member "CustomerMasterKeySpec" xml)
            CustomerMasterKeySpec.parse
      ; key_spec = Aws.Util.option_bind (Aws.Xml.member "KeySpec" xml) KeySpec.parse
      ; encryption_algorithms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "EncryptionAlgorithms" xml)
               EncryptionAlgorithmSpecList.parse)
      ; signing_algorithms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SigningAlgorithms" xml)
               SigningAlgorithmSpecList.parse)
      ; multi_region =
          Aws.Util.option_bind (Aws.Xml.member "MultiRegion" xml) Boolean.parse
      ; multi_region_configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "MultiRegionConfiguration" xml)
            MultiRegionConfiguration.parse
      ; pending_deletion_window_in_days =
          Aws.Util.option_bind
            (Aws.Xml.member "PendingDeletionWindowInDays" xml)
            Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.pending_deletion_window_in_days (fun f ->
               Aws.Query.Pair ("PendingDeletionWindowInDays", Integer.to_query f))
         ; Aws.Util.option_map v.multi_region_configuration (fun f ->
               Aws.Query.Pair
                 ("MultiRegionConfiguration", MultiRegionConfiguration.to_query f))
         ; Aws.Util.option_map v.multi_region (fun f ->
               Aws.Query.Pair ("MultiRegion", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "SigningAlgorithms.member"
                , SigningAlgorithmSpecList.to_query v.signing_algorithms ))
         ; Some
             (Aws.Query.Pair
                ( "EncryptionAlgorithms.member"
                , EncryptionAlgorithmSpecList.to_query v.encryption_algorithms ))
         ; Aws.Util.option_map v.key_spec (fun f ->
               Aws.Query.Pair ("KeySpec", KeySpec.to_query f))
         ; Aws.Util.option_map v.customer_master_key_spec (fun f ->
               Aws.Query.Pair ("CustomerMasterKeySpec", CustomerMasterKeySpec.to_query f))
         ; Aws.Util.option_map v.key_manager (fun f ->
               Aws.Query.Pair ("KeyManager", KeyManagerType.to_query f))
         ; Aws.Util.option_map v.expiration_model (fun f ->
               Aws.Query.Pair ("ExpirationModel", ExpirationModelType.to_query f))
         ; Aws.Util.option_map v.cloud_hsm_cluster_id (fun f ->
               Aws.Query.Pair ("CloudHsmClusterId", String.to_query f))
         ; Aws.Util.option_map v.custom_key_store_id (fun f ->
               Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
         ; Aws.Util.option_map v.origin (fun f ->
               Aws.Query.Pair ("Origin", OriginType.to_query f))
         ; Aws.Util.option_map v.valid_to (fun f ->
               Aws.Query.Pair ("ValidTo", DateTime.to_query f))
         ; Aws.Util.option_map v.deletion_date (fun f ->
               Aws.Query.Pair ("DeletionDate", DateTime.to_query f))
         ; Aws.Util.option_map v.key_state (fun f ->
               Aws.Query.Pair ("KeyState", KeyState.to_query f))
         ; Aws.Util.option_map v.key_usage (fun f ->
               Aws.Query.Pair ("KeyUsage", KeyUsageType.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.enabled (fun f ->
               Aws.Query.Pair ("Enabled", Boolean.to_query f))
         ; Aws.Util.option_map v.creation_date (fun f ->
               Aws.Query.Pair ("CreationDate", DateTime.to_query f))
         ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ; Aws.Util.option_map v.a_w_s_account_id (fun f ->
               Aws.Query.Pair ("AWSAccountId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.pending_deletion_window_in_days (fun f ->
               "PendingDeletionWindowInDays", Integer.to_json f)
         ; Aws.Util.option_map v.multi_region_configuration (fun f ->
               "MultiRegionConfiguration", MultiRegionConfiguration.to_json f)
         ; Aws.Util.option_map v.multi_region (fun f -> "MultiRegion", Boolean.to_json f)
         ; Some
             ("SigningAlgorithms", SigningAlgorithmSpecList.to_json v.signing_algorithms)
         ; Some
             ( "EncryptionAlgorithms"
             , EncryptionAlgorithmSpecList.to_json v.encryption_algorithms )
         ; Aws.Util.option_map v.key_spec (fun f -> "KeySpec", KeySpec.to_json f)
         ; Aws.Util.option_map v.customer_master_key_spec (fun f ->
               "CustomerMasterKeySpec", CustomerMasterKeySpec.to_json f)
         ; Aws.Util.option_map v.key_manager (fun f ->
               "KeyManager", KeyManagerType.to_json f)
         ; Aws.Util.option_map v.expiration_model (fun f ->
               "ExpirationModel", ExpirationModelType.to_json f)
         ; Aws.Util.option_map v.cloud_hsm_cluster_id (fun f ->
               "CloudHsmClusterId", String.to_json f)
         ; Aws.Util.option_map v.custom_key_store_id (fun f ->
               "CustomKeyStoreId", String.to_json f)
         ; Aws.Util.option_map v.origin (fun f -> "Origin", OriginType.to_json f)
         ; Aws.Util.option_map v.valid_to (fun f -> "ValidTo", DateTime.to_json f)
         ; Aws.Util.option_map v.deletion_date (fun f ->
               "DeletionDate", DateTime.to_json f)
         ; Aws.Util.option_map v.key_state (fun f -> "KeyState", KeyState.to_json f)
         ; Aws.Util.option_map v.key_usage (fun f -> "KeyUsage", KeyUsageType.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
         ; Aws.Util.option_map v.creation_date (fun f ->
               "CreationDate", DateTime.to_json f)
         ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
         ; Some ("KeyId", String.to_json v.key_id)
         ; Aws.Util.option_map v.a_w_s_account_id (fun f ->
               "AWSAccountId", String.to_json f)
         ])

  let of_json j =
    { a_w_s_account_id =
        Aws.Util.option_map (Aws.Json.lookup j "AWSAccountId") String.of_json
    ; key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
    ; creation_date =
        Aws.Util.option_map (Aws.Json.lookup j "CreationDate") DateTime.of_json
    ; enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; key_usage = Aws.Util.option_map (Aws.Json.lookup j "KeyUsage") KeyUsageType.of_json
    ; key_state = Aws.Util.option_map (Aws.Json.lookup j "KeyState") KeyState.of_json
    ; deletion_date =
        Aws.Util.option_map (Aws.Json.lookup j "DeletionDate") DateTime.of_json
    ; valid_to = Aws.Util.option_map (Aws.Json.lookup j "ValidTo") DateTime.of_json
    ; origin = Aws.Util.option_map (Aws.Json.lookup j "Origin") OriginType.of_json
    ; custom_key_store_id =
        Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
    ; cloud_hsm_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CloudHsmClusterId") String.of_json
    ; expiration_model =
        Aws.Util.option_map
          (Aws.Json.lookup j "ExpirationModel")
          ExpirationModelType.of_json
    ; key_manager =
        Aws.Util.option_map (Aws.Json.lookup j "KeyManager") KeyManagerType.of_json
    ; customer_master_key_spec =
        Aws.Util.option_map
          (Aws.Json.lookup j "CustomerMasterKeySpec")
          CustomerMasterKeySpec.of_json
    ; key_spec = Aws.Util.option_map (Aws.Json.lookup j "KeySpec") KeySpec.of_json
    ; encryption_algorithms =
        EncryptionAlgorithmSpecList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EncryptionAlgorithms"))
    ; signing_algorithms =
        SigningAlgorithmSpecList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SigningAlgorithms"))
    ; multi_region = Aws.Util.option_map (Aws.Json.lookup j "MultiRegion") Boolean.of_json
    ; multi_region_configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "MultiRegionConfiguration")
          MultiRegionConfiguration.of_json
    ; pending_deletion_window_in_days =
        Aws.Util.option_map
          (Aws.Json.lookup j "PendingDeletionWindowInDays")
          Integer.of_json
    }
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
          Aws.Util.option_bind (Aws.Xml.member "ReplicaKeyMetadata" xml) KeyMetadata.parse
      ; replica_policy =
          Aws.Util.option_bind (Aws.Xml.member "ReplicaPolicy" xml) String.parse
      ; replica_tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "ReplicaTags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ReplicaTags.member", TagList.to_query v.replica_tags))
         ; Aws.Util.option_map v.replica_policy (fun f ->
               Aws.Query.Pair ("ReplicaPolicy", String.to_query f))
         ; Aws.Util.option_map v.replica_key_metadata (fun f ->
               Aws.Query.Pair ("ReplicaKeyMetadata", KeyMetadata.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ReplicaTags", TagList.to_json v.replica_tags)
         ; Aws.Util.option_map v.replica_policy (fun f ->
               "ReplicaPolicy", String.to_json f)
         ; Aws.Util.option_map v.replica_key_metadata (fun f ->
               "ReplicaKeyMetadata", KeyMetadata.to_json f)
         ])

  let of_json j =
    { replica_key_metadata =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicaKeyMetadata") KeyMetadata.of_json
    ; replica_policy =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicaPolicy") String.of_json
    ; replica_tags =
        TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaTags"))
    }
end

module CreateKeyResponse = struct
  type t = { key_metadata : KeyMetadata.t option }

  let make ?key_metadata () = { key_metadata }

  let parse xml =
    Some
      { key_metadata =
          Aws.Util.option_bind (Aws.Xml.member "KeyMetadata" xml) KeyMetadata.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_metadata (fun f ->
               Aws.Query.Pair ("KeyMetadata", KeyMetadata.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_metadata (fun f ->
               "KeyMetadata", KeyMetadata.to_json f)
         ])

  let of_json j =
    { key_metadata =
        Aws.Util.option_map (Aws.Json.lookup j "KeyMetadata") KeyMetadata.of_json
    }
end

module DisableKeyRotationRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("KeyId", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId")) }
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
      { ciphertext_blob =
          Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse
      ; source_key_id =
          Aws.Util.option_bind (Aws.Xml.member "SourceKeyId" xml) String.parse
      ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; source_encryption_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "SourceEncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      ; destination_encryption_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "DestinationEncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.destination_encryption_algorithm (fun f ->
               Aws.Query.Pair
                 ("DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Aws.Util.option_map v.source_encryption_algorithm (fun f ->
               Aws.Query.Pair
                 ("SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ; Aws.Util.option_map v.source_key_id (fun f ->
               Aws.Query.Pair ("SourceKeyId", String.to_query f))
         ; Aws.Util.option_map v.ciphertext_blob (fun f ->
               Aws.Query.Pair ("CiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.destination_encryption_algorithm (fun f ->
               "DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
         ; Aws.Util.option_map v.source_encryption_algorithm (fun f ->
               "SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ; Aws.Util.option_map v.source_key_id (fun f -> "SourceKeyId", String.to_json f)
         ; Aws.Util.option_map v.ciphertext_blob (fun f ->
               "CiphertextBlob", Blob.to_json f)
         ])

  let of_json j =
    { ciphertext_blob =
        Aws.Util.option_map (Aws.Json.lookup j "CiphertextBlob") Blob.of_json
    ; source_key_id = Aws.Util.option_map (Aws.Json.lookup j "SourceKeyId") String.of_json
    ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; source_encryption_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "SourceEncryptionAlgorithm")
          EncryptionAlgorithmSpec.of_json
    ; destination_encryption_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "DestinationEncryptionAlgorithm")
          EncryptionAlgorithmSpec.of_json
    }
end

module MessageType = struct
  type t =
    | RAW
    | DIGEST

  let str_to_t = [ "DIGEST", DIGEST; "RAW", RAW ]

  let t_to_str = [ DIGEST, "DIGEST"; RAW, "RAW" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module CancelKeyDeletionResponse = struct
  type t = { key_id : String.t option }

  let make ?key_id () = { key_id }

  let parse xml =
    Some { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f) ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json }
end

module DisableKeyRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("KeyId", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId")) }
end

module GrantTokenList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    }
end

module TagException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module InvalidGrantIdException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
      { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; import_token = Aws.Util.option_bind (Aws.Xml.member "ImportToken" xml) Blob.parse
      ; public_key = Aws.Util.option_bind (Aws.Xml.member "PublicKey" xml) Blob.parse
      ; parameters_valid_to =
          Aws.Util.option_bind (Aws.Xml.member "ParametersValidTo" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.parameters_valid_to (fun f ->
               Aws.Query.Pair ("ParametersValidTo", DateTime.to_query f))
         ; Aws.Util.option_map v.public_key (fun f ->
               Aws.Query.Pair ("PublicKey", Blob.to_query f))
         ; Aws.Util.option_map v.import_token (fun f ->
               Aws.Query.Pair ("ImportToken", Blob.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.parameters_valid_to (fun f ->
               "ParametersValidTo", DateTime.to_json f)
         ; Aws.Util.option_map v.public_key (fun f -> "PublicKey", Blob.to_json f)
         ; Aws.Util.option_map v.import_token (fun f -> "ImportToken", Blob.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; import_token = Aws.Util.option_map (Aws.Json.lookup j "ImportToken") Blob.of_json
    ; public_key = Aws.Util.option_map (Aws.Json.lookup j "PublicKey") Blob.of_json
    ; parameters_valid_to =
        Aws.Util.option_map (Aws.Json.lookup j "ParametersValidTo") DateTime.of_json
    }
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
      { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; deletion_date =
          Aws.Util.option_bind (Aws.Xml.member "DeletionDate" xml) DateTime.parse
      ; key_state = Aws.Util.option_bind (Aws.Xml.member "KeyState" xml) KeyState.parse
      ; pending_window_in_days =
          Aws.Util.option_bind (Aws.Xml.member "PendingWindowInDays" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.pending_window_in_days (fun f ->
               Aws.Query.Pair ("PendingWindowInDays", Integer.to_query f))
         ; Aws.Util.option_map v.key_state (fun f ->
               Aws.Query.Pair ("KeyState", KeyState.to_query f))
         ; Aws.Util.option_map v.deletion_date (fun f ->
               Aws.Query.Pair ("DeletionDate", DateTime.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.pending_window_in_days (fun f ->
               "PendingWindowInDays", Integer.to_json f)
         ; Aws.Util.option_map v.key_state (fun f -> "KeyState", KeyState.to_json f)
         ; Aws.Util.option_map v.deletion_date (fun f ->
               "DeletionDate", DateTime.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; deletion_date =
        Aws.Util.option_map (Aws.Json.lookup j "DeletionDate") DateTime.of_json
    ; key_state = Aws.Util.option_map (Aws.Json.lookup j "KeyState") KeyState.of_json
    ; pending_window_in_days =
        Aws.Util.option_map (Aws.Json.lookup j "PendingWindowInDays") Integer.of_json
    }
end

module IncorrectTrustAnchorException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module TagKeyList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; tag_keys =
          Aws.Xml.required
            "TagKeys"
            (Aws.Util.option_bind (Aws.Xml.member "TagKeys" xml) TagKeyList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagKeys.member", TagKeyList.to_query v.tag_keys))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagKeys", TagKeyList.to_json v.tag_keys)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; tag_keys = TagKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKeys"))
    }
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module GrantOperationList = struct
  type t = GrantOperation.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map GrantOperation.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list GrantOperation.to_query v

  let to_json v = `List (List.map GrantOperation.to_json v)

  let of_json j = Aws.Json.to_list GrantOperation.of_json j
end

module EncryptionContextType = struct
  type t = (String.t, String.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v = Aws.Query.to_query_hashtbl String.to_string String.to_query v

  let to_json v =
    `Assoc
      (Hashtbl.fold (fun k v acc -> (String.to_string k, String.to_json v) :: acc) v [])

  let of_json j = Aws.Json.to_hashtbl String.of_string String.of_json j
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
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionContextSubset" xml)
            EncryptionContextType.parse
      ; encryption_context_equals =
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionContextEquals" xml)
            EncryptionContextType.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_context_equals (fun f ->
               Aws.Query.Pair ("EncryptionContextEquals", EncryptionContextType.to_query f))
         ; Aws.Util.option_map v.encryption_context_subset (fun f ->
               Aws.Query.Pair ("EncryptionContextSubset", EncryptionContextType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_context_equals (fun f ->
               "EncryptionContextEquals", EncryptionContextType.to_json f)
         ; Aws.Util.option_map v.encryption_context_subset (fun f ->
               "EncryptionContextSubset", EncryptionContextType.to_json f)
         ])

  let of_json j =
    { encryption_context_subset =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionContextSubset")
          EncryptionContextType.of_json
    ; encryption_context_equals =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionContextEquals")
          EncryptionContextType.of_json
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
      { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; grant_id = Aws.Util.option_bind (Aws.Xml.member "GrantId" xml) String.parse
      ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; creation_date =
          Aws.Util.option_bind (Aws.Xml.member "CreationDate" xml) DateTime.parse
      ; grantee_principal =
          Aws.Util.option_bind (Aws.Xml.member "GranteePrincipal" xml) String.parse
      ; retiring_principal =
          Aws.Util.option_bind (Aws.Xml.member "RetiringPrincipal" xml) String.parse
      ; issuing_account =
          Aws.Util.option_bind (Aws.Xml.member "IssuingAccount" xml) String.parse
      ; operations =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Operations" xml)
               GrantOperationList.parse)
      ; constraints =
          Aws.Util.option_bind (Aws.Xml.member "Constraints" xml) GrantConstraints.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.constraints (fun f ->
               Aws.Query.Pair ("Constraints", GrantConstraints.to_query f))
         ; Some
             (Aws.Query.Pair
                ("Operations.member", GrantOperationList.to_query v.operations))
         ; Aws.Util.option_map v.issuing_account (fun f ->
               Aws.Query.Pair ("IssuingAccount", String.to_query f))
         ; Aws.Util.option_map v.retiring_principal (fun f ->
               Aws.Query.Pair ("RetiringPrincipal", String.to_query f))
         ; Aws.Util.option_map v.grantee_principal (fun f ->
               Aws.Query.Pair ("GranteePrincipal", String.to_query f))
         ; Aws.Util.option_map v.creation_date (fun f ->
               Aws.Query.Pair ("CreationDate", DateTime.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ; Aws.Util.option_map v.grant_id (fun f ->
               Aws.Query.Pair ("GrantId", String.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.constraints (fun f ->
               "Constraints", GrantConstraints.to_json f)
         ; Some ("Operations", GrantOperationList.to_json v.operations)
         ; Aws.Util.option_map v.issuing_account (fun f ->
               "IssuingAccount", String.to_json f)
         ; Aws.Util.option_map v.retiring_principal (fun f ->
               "RetiringPrincipal", String.to_json f)
         ; Aws.Util.option_map v.grantee_principal (fun f ->
               "GranteePrincipal", String.to_json f)
         ; Aws.Util.option_map v.creation_date (fun f ->
               "CreationDate", DateTime.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ; Aws.Util.option_map v.grant_id (fun f -> "GrantId", String.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; grant_id = Aws.Util.option_map (Aws.Json.lookup j "GrantId") String.of_json
    ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; creation_date =
        Aws.Util.option_map (Aws.Json.lookup j "CreationDate") DateTime.of_json
    ; grantee_principal =
        Aws.Util.option_map (Aws.Json.lookup j "GranteePrincipal") String.of_json
    ; retiring_principal =
        Aws.Util.option_map (Aws.Json.lookup j "RetiringPrincipal") String.of_json
    ; issuing_account =
        Aws.Util.option_map (Aws.Json.lookup j "IssuingAccount") String.of_json
    ; operations =
        GrantOperationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Operations"))
    ; constraints =
        Aws.Util.option_map (Aws.Json.lookup j "Constraints") GrantConstraints.of_json
    }
end

module GrantList = struct
  type t = GrantListEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map GrantListEntry.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list GrantListEntry.to_query v

  let to_json v = `List (List.map GrantListEntry.to_json v)

  let of_json j = Aws.Json.to_list GrantListEntry.of_json j
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
          Aws.Xml.required
            "CustomKeyStoreName"
            (Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreName" xml) String.parse)
      ; cloud_hsm_cluster_id =
          Aws.Xml.required
            "CloudHsmClusterId"
            (Aws.Util.option_bind (Aws.Xml.member "CloudHsmClusterId" xml) String.parse)
      ; trust_anchor_certificate =
          Aws.Xml.required
            "TrustAnchorCertificate"
            (Aws.Util.option_bind
               (Aws.Xml.member "TrustAnchorCertificate" xml)
               String.parse)
      ; key_store_password =
          Aws.Xml.required
            "KeyStorePassword"
            (Aws.Util.option_bind (Aws.Xml.member "KeyStorePassword" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("KeyStorePassword", String.to_query v.key_store_password))
         ; Some
             (Aws.Query.Pair
                ("TrustAnchorCertificate", String.to_query v.trust_anchor_certificate))
         ; Some
             (Aws.Query.Pair ("CloudHsmClusterId", String.to_query v.cloud_hsm_cluster_id))
         ; Some
             (Aws.Query.Pair
                ("CustomKeyStoreName", String.to_query v.custom_key_store_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("KeyStorePassword", String.to_json v.key_store_password)
         ; Some ("TrustAnchorCertificate", String.to_json v.trust_anchor_certificate)
         ; Some ("CloudHsmClusterId", String.to_json v.cloud_hsm_cluster_id)
         ; Some ("CustomKeyStoreName", String.to_json v.custom_key_store_name)
         ])

  let of_json j =
    { custom_key_store_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomKeyStoreName"))
    ; cloud_hsm_cluster_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CloudHsmClusterId"))
    ; trust_anchor_certificate =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TrustAnchorCertificate"))
    ; key_store_password =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyStorePassword"))
    }
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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
          Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
      ; custom_key_store_name =
          Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreName" xml) String.parse
      ; cloud_hsm_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CloudHsmClusterId" xml) String.parse
      ; trust_anchor_certificate =
          Aws.Util.option_bind (Aws.Xml.member "TrustAnchorCertificate" xml) String.parse
      ; connection_state =
          Aws.Util.option_bind
            (Aws.Xml.member "ConnectionState" xml)
            ConnectionStateType.parse
      ; connection_error_code =
          Aws.Util.option_bind
            (Aws.Xml.member "ConnectionErrorCode" xml)
            ConnectionErrorCodeType.parse
      ; creation_date =
          Aws.Util.option_bind (Aws.Xml.member "CreationDate" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.creation_date (fun f ->
               Aws.Query.Pair ("CreationDate", DateTime.to_query f))
         ; Aws.Util.option_map v.connection_error_code (fun f ->
               Aws.Query.Pair ("ConnectionErrorCode", ConnectionErrorCodeType.to_query f))
         ; Aws.Util.option_map v.connection_state (fun f ->
               Aws.Query.Pair ("ConnectionState", ConnectionStateType.to_query f))
         ; Aws.Util.option_map v.trust_anchor_certificate (fun f ->
               Aws.Query.Pair ("TrustAnchorCertificate", String.to_query f))
         ; Aws.Util.option_map v.cloud_hsm_cluster_id (fun f ->
               Aws.Query.Pair ("CloudHsmClusterId", String.to_query f))
         ; Aws.Util.option_map v.custom_key_store_name (fun f ->
               Aws.Query.Pair ("CustomKeyStoreName", String.to_query f))
         ; Aws.Util.option_map v.custom_key_store_id (fun f ->
               Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.creation_date (fun f ->
               "CreationDate", DateTime.to_json f)
         ; Aws.Util.option_map v.connection_error_code (fun f ->
               "ConnectionErrorCode", ConnectionErrorCodeType.to_json f)
         ; Aws.Util.option_map v.connection_state (fun f ->
               "ConnectionState", ConnectionStateType.to_json f)
         ; Aws.Util.option_map v.trust_anchor_certificate (fun f ->
               "TrustAnchorCertificate", String.to_json f)
         ; Aws.Util.option_map v.cloud_hsm_cluster_id (fun f ->
               "CloudHsmClusterId", String.to_json f)
         ; Aws.Util.option_map v.custom_key_store_name (fun f ->
               "CustomKeyStoreName", String.to_json f)
         ; Aws.Util.option_map v.custom_key_store_id (fun f ->
               "CustomKeyStoreId", String.to_json f)
         ])

  let of_json j =
    { custom_key_store_id =
        Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
    ; custom_key_store_name =
        Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreName") String.of_json
    ; cloud_hsm_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CloudHsmClusterId") String.of_json
    ; trust_anchor_certificate =
        Aws.Util.option_map (Aws.Json.lookup j "TrustAnchorCertificate") String.of_json
    ; connection_state =
        Aws.Util.option_map
          (Aws.Json.lookup j "ConnectionState")
          ConnectionStateType.of_json
    ; connection_error_code =
        Aws.Util.option_map
          (Aws.Json.lookup j "ConnectionErrorCode")
          ConnectionErrorCodeType.of_json
    ; creation_date =
        Aws.Util.option_map (Aws.Json.lookup j "CreationDate") DateTime.of_json
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
      { alias_name = Aws.Util.option_bind (Aws.Xml.member "AliasName" xml) String.parse
      ; alias_arn = Aws.Util.option_bind (Aws.Xml.member "AliasArn" xml) String.parse
      ; target_key_id =
          Aws.Util.option_bind (Aws.Xml.member "TargetKeyId" xml) String.parse
      ; creation_date =
          Aws.Util.option_bind (Aws.Xml.member "CreationDate" xml) DateTime.parse
      ; last_updated_date =
          Aws.Util.option_bind (Aws.Xml.member "LastUpdatedDate" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_updated_date (fun f ->
               Aws.Query.Pair ("LastUpdatedDate", DateTime.to_query f))
         ; Aws.Util.option_map v.creation_date (fun f ->
               Aws.Query.Pair ("CreationDate", DateTime.to_query f))
         ; Aws.Util.option_map v.target_key_id (fun f ->
               Aws.Query.Pair ("TargetKeyId", String.to_query f))
         ; Aws.Util.option_map v.alias_arn (fun f ->
               Aws.Query.Pair ("AliasArn", String.to_query f))
         ; Aws.Util.option_map v.alias_name (fun f ->
               Aws.Query.Pair ("AliasName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_updated_date (fun f ->
               "LastUpdatedDate", DateTime.to_json f)
         ; Aws.Util.option_map v.creation_date (fun f ->
               "CreationDate", DateTime.to_json f)
         ; Aws.Util.option_map v.target_key_id (fun f -> "TargetKeyId", String.to_json f)
         ; Aws.Util.option_map v.alias_arn (fun f -> "AliasArn", String.to_json f)
         ; Aws.Util.option_map v.alias_name (fun f -> "AliasName", String.to_json f)
         ])

  let of_json j =
    { alias_name = Aws.Util.option_map (Aws.Json.lookup j "AliasName") String.of_json
    ; alias_arn = Aws.Util.option_map (Aws.Json.lookup j "AliasArn") String.of_json
    ; target_key_id = Aws.Util.option_map (Aws.Json.lookup j "TargetKeyId") String.of_json
    ; creation_date =
        Aws.Util.option_map (Aws.Json.lookup j "CreationDate") DateTime.of_json
    ; last_updated_date =
        Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedDate") DateTime.of_json
    }
end

module AliasList = struct
  type t = AliasListEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map AliasListEntry.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AliasListEntry.to_query v

  let to_json v = `List (List.map AliasListEntry.to_json v)

  let of_json j = Aws.Json.to_list AliasListEntry.of_json j
end

module InvalidAliasNameException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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
          Aws.Util.option_bind (Aws.Xml.member "PrivateKeyCiphertextBlob" xml) Blob.parse
      ; public_key = Aws.Util.option_bind (Aws.Xml.member "PublicKey" xml) Blob.parse
      ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; key_pair_spec =
          Aws.Util.option_bind (Aws.Xml.member "KeyPairSpec" xml) DataKeyPairSpec.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_pair_spec (fun f ->
               Aws.Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ; Aws.Util.option_map v.public_key (fun f ->
               Aws.Query.Pair ("PublicKey", Blob.to_query f))
         ; Aws.Util.option_map v.private_key_ciphertext_blob (fun f ->
               Aws.Query.Pair ("PrivateKeyCiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_pair_spec (fun f ->
               "KeyPairSpec", DataKeyPairSpec.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ; Aws.Util.option_map v.public_key (fun f -> "PublicKey", Blob.to_json f)
         ; Aws.Util.option_map v.private_key_ciphertext_blob (fun f ->
               "PrivateKeyCiphertextBlob", Blob.to_json f)
         ])

  let of_json j =
    { private_key_ciphertext_blob =
        Aws.Util.option_map (Aws.Json.lookup j "PrivateKeyCiphertextBlob") Blob.of_json
    ; public_key = Aws.Util.option_map (Aws.Json.lookup j "PublicKey") Blob.of_json
    ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; key_pair_spec =
        Aws.Util.option_map (Aws.Json.lookup j "KeyPairSpec") DataKeyPairSpec.of_json
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
      { policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; key_usage =
          Aws.Util.option_bind (Aws.Xml.member "KeyUsage" xml) KeyUsageType.parse
      ; customer_master_key_spec =
          Aws.Util.option_bind
            (Aws.Xml.member "CustomerMasterKeySpec" xml)
            CustomerMasterKeySpec.parse
      ; key_spec = Aws.Util.option_bind (Aws.Xml.member "KeySpec" xml) KeySpec.parse
      ; origin = Aws.Util.option_bind (Aws.Xml.member "Origin" xml) OriginType.parse
      ; custom_key_store_id =
          Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
      ; bypass_policy_lockout_safety_check =
          Aws.Util.option_bind
            (Aws.Xml.member "BypassPolicyLockoutSafetyCheck" xml)
            Boolean.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      ; multi_region =
          Aws.Util.option_bind (Aws.Xml.member "MultiRegion" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.multi_region (fun f ->
               Aws.Query.Pair ("MultiRegion", Boolean.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               Aws.Query.Pair ("BypassPolicyLockoutSafetyCheck", Boolean.to_query f))
         ; Aws.Util.option_map v.custom_key_store_id (fun f ->
               Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
         ; Aws.Util.option_map v.origin (fun f ->
               Aws.Query.Pair ("Origin", OriginType.to_query f))
         ; Aws.Util.option_map v.key_spec (fun f ->
               Aws.Query.Pair ("KeySpec", KeySpec.to_query f))
         ; Aws.Util.option_map v.customer_master_key_spec (fun f ->
               Aws.Query.Pair ("CustomerMasterKeySpec", CustomerMasterKeySpec.to_query f))
         ; Aws.Util.option_map v.key_usage (fun f ->
               Aws.Query.Pair ("KeyUsage", KeyUsageType.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.policy (fun f ->
               Aws.Query.Pair ("Policy", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.multi_region (fun f -> "MultiRegion", Boolean.to_json f)
         ; Some ("Tags", TagList.to_json v.tags)
         ; Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               "BypassPolicyLockoutSafetyCheck", Boolean.to_json f)
         ; Aws.Util.option_map v.custom_key_store_id (fun f ->
               "CustomKeyStoreId", String.to_json f)
         ; Aws.Util.option_map v.origin (fun f -> "Origin", OriginType.to_json f)
         ; Aws.Util.option_map v.key_spec (fun f -> "KeySpec", KeySpec.to_json f)
         ; Aws.Util.option_map v.customer_master_key_spec (fun f ->
               "CustomerMasterKeySpec", CustomerMasterKeySpec.to_json f)
         ; Aws.Util.option_map v.key_usage (fun f -> "KeyUsage", KeyUsageType.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
         ])

  let of_json j =
    { policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; key_usage = Aws.Util.option_map (Aws.Json.lookup j "KeyUsage") KeyUsageType.of_json
    ; customer_master_key_spec =
        Aws.Util.option_map
          (Aws.Json.lookup j "CustomerMasterKeySpec")
          CustomerMasterKeySpec.of_json
    ; key_spec = Aws.Util.option_map (Aws.Json.lookup j "KeySpec") KeySpec.of_json
    ; origin = Aws.Util.option_map (Aws.Json.lookup j "Origin") OriginType.of_json
    ; custom_key_store_id =
        Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
    ; bypass_policy_lockout_safety_check =
        Aws.Util.option_map
          (Aws.Json.lookup j "BypassPolicyLockoutSafetyCheck")
          Boolean.of_json
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; multi_region = Aws.Util.option_map (Aws.Json.lookup j "MultiRegion") Boolean.of_json
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; description =
          Aws.Xml.required
            "Description"
            (Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Description", String.to_query v.description))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Description", String.to_json v.description)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; description =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Description"))
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
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Aliases" xml) AliasList.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      ; truncated = Aws.Util.option_bind (Aws.Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f ->
               Aws.Query.Pair ("Truncated", Boolean.to_query f))
         ; Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some (Aws.Query.Pair ("Aliases.member", AliasList.to_query v.aliases))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
         ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some ("Aliases", AliasList.to_json v.aliases)
         ])

  let of_json j =
    { aliases = AliasList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Aliases"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
    }
end

module DisconnectCustomKeyStoreRequest = struct
  type t = { custom_key_store_id : String.t }

  let make ~custom_key_store_id () = { custom_key_store_id }

  let parse xml =
    Some
      { custom_key_store_id =
          Aws.Xml.required
            "CustomKeyStoreId"
            (Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CustomKeyStoreId", String.to_json v.custom_key_store_id) ])

  let of_json j =
    { custom_key_store_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomKeyStoreId"))
    }
end

module ConnectCustomKeyStoreResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module WrappingKeySpec = struct
  type t = RSA_2048

  let str_to_t = [ "RSA_2048", RSA_2048 ]

  let t_to_str = [ RSA_2048, "RSA_2048" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module GetKeyRotationStatusResponse = struct
  type t = { key_rotation_enabled : Boolean.t option }

  let make ?key_rotation_enabled () = { key_rotation_enabled }

  let parse xml =
    Some
      { key_rotation_enabled =
          Aws.Util.option_bind (Aws.Xml.member "KeyRotationEnabled" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_rotation_enabled (fun f ->
               Aws.Query.Pair ("KeyRotationEnabled", Boolean.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_rotation_enabled (fun f ->
               "KeyRotationEnabled", Boolean.to_json f)
         ])

  let of_json j =
    { key_rotation_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "KeyRotationEnabled") Boolean.of_json
    }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; replica_region =
          Aws.Xml.required
            "ReplicaRegion"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicaRegion" xml) String.parse)
      ; policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
      ; bypass_policy_lockout_safety_check =
          Aws.Util.option_bind
            (Aws.Xml.member "BypassPolicyLockoutSafetyCheck" xml)
            Boolean.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               Aws.Query.Pair ("BypassPolicyLockoutSafetyCheck", Boolean.to_query f))
         ; Aws.Util.option_map v.policy (fun f ->
               Aws.Query.Pair ("Policy", String.to_query f))
         ; Some (Aws.Query.Pair ("ReplicaRegion", String.to_query v.replica_region))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               "BypassPolicyLockoutSafetyCheck", Boolean.to_json f)
         ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
         ; Some ("ReplicaRegion", String.to_json v.replica_region)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; replica_region =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaRegion"))
    ; policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
    ; bypass_policy_lockout_safety_check =
        Aws.Util.option_map
          (Aws.Json.lookup j "BypassPolicyLockoutSafetyCheck")
          Boolean.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module LimitExceededException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GenerateDataKeyWithoutPlaintextResponse = struct
  type t =
    { ciphertext_blob : Blob.t option
    ; key_id : String.t option
    }

  let make ?ciphertext_blob ?key_id () = { ciphertext_blob; key_id }

  let parse xml =
    Some
      { ciphertext_blob =
          Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse
      ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ; Aws.Util.option_map v.ciphertext_blob (fun f ->
               Aws.Query.Pair ("CiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ; Aws.Util.option_map v.ciphertext_blob (fun f ->
               "CiphertextBlob", Blob.to_json f)
         ])

  let of_json j =
    { ciphertext_blob =
        Aws.Util.option_map (Aws.Json.lookup j "CiphertextBlob") Blob.of_json
    ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; grant_id =
          Aws.Xml.required
            "GrantId"
            (Aws.Util.option_bind (Aws.Xml.member "GrantId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("GrantId", String.to_query v.grant_id))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GrantId", String.to_json v.grant_id)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; grant_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantId"))
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
      { limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; grant_id = Aws.Util.option_bind (Aws.Xml.member "GrantId" xml) String.parse
      ; grantee_principal =
          Aws.Util.option_bind (Aws.Xml.member "GranteePrincipal" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.grantee_principal (fun f ->
               Aws.Query.Pair ("GranteePrincipal", String.to_query f))
         ; Aws.Util.option_map v.grant_id (fun f ->
               Aws.Query.Pair ("GrantId", String.to_query f))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.limit (fun f ->
               Aws.Query.Pair ("Limit", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.grantee_principal (fun f ->
               "GranteePrincipal", String.to_json f)
         ; Aws.Util.option_map v.grant_id (fun f -> "GrantId", String.to_json f)
         ; Some ("KeyId", String.to_json v.key_id)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
         ])

  let of_json j =
    { limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; grant_id = Aws.Util.option_map (Aws.Json.lookup j "GrantId") String.of_json
    ; grantee_principal =
        Aws.Util.option_map (Aws.Json.lookup j "GranteePrincipal") String.of_json
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
      { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; key_arn = Aws.Util.option_bind (Aws.Xml.member "KeyArn" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_arn (fun f ->
               Aws.Query.Pair ("KeyArn", String.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_arn (fun f -> "KeyArn", String.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; key_arn = Aws.Util.option_map (Aws.Json.lookup j "KeyArn") String.of_json
    }
end

module KeyList = struct
  type t = KeyListEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map KeyListEntry.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list KeyListEntry.to_query v

  let to_json v = `List (List.map KeyListEntry.to_json v)

  let of_json j = Aws.Json.to_list KeyListEntry.of_json j
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
          Aws.Util.option_bind (Aws.Xml.member "PrivateKeyCiphertextBlob" xml) Blob.parse
      ; private_key_plaintext =
          Aws.Util.option_bind (Aws.Xml.member "PrivateKeyPlaintext" xml) Blob.parse
      ; public_key = Aws.Util.option_bind (Aws.Xml.member "PublicKey" xml) Blob.parse
      ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; key_pair_spec =
          Aws.Util.option_bind (Aws.Xml.member "KeyPairSpec" xml) DataKeyPairSpec.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_pair_spec (fun f ->
               Aws.Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ; Aws.Util.option_map v.public_key (fun f ->
               Aws.Query.Pair ("PublicKey", Blob.to_query f))
         ; Aws.Util.option_map v.private_key_plaintext (fun f ->
               Aws.Query.Pair ("PrivateKeyPlaintext", Blob.to_query f))
         ; Aws.Util.option_map v.private_key_ciphertext_blob (fun f ->
               Aws.Query.Pair ("PrivateKeyCiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_pair_spec (fun f ->
               "KeyPairSpec", DataKeyPairSpec.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ; Aws.Util.option_map v.public_key (fun f -> "PublicKey", Blob.to_json f)
         ; Aws.Util.option_map v.private_key_plaintext (fun f ->
               "PrivateKeyPlaintext", Blob.to_json f)
         ; Aws.Util.option_map v.private_key_ciphertext_blob (fun f ->
               "PrivateKeyCiphertextBlob", Blob.to_json f)
         ])

  let of_json j =
    { private_key_ciphertext_blob =
        Aws.Util.option_map (Aws.Json.lookup j "PrivateKeyCiphertextBlob") Blob.of_json
    ; private_key_plaintext =
        Aws.Util.option_map (Aws.Json.lookup j "PrivateKeyPlaintext") Blob.of_json
    ; public_key = Aws.Util.option_map (Aws.Json.lookup j "PublicKey") Blob.of_json
    ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; key_pair_spec =
        Aws.Util.option_map (Aws.Json.lookup j "KeyPairSpec") DataKeyPairSpec.of_json
    }
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
          Aws.Xml.required
            "AliasName"
            (Aws.Util.option_bind (Aws.Xml.member "AliasName" xml) String.parse)
      ; target_key_id =
          Aws.Xml.required
            "TargetKeyId"
            (Aws.Util.option_bind (Aws.Xml.member "TargetKeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TargetKeyId", String.to_query v.target_key_id))
         ; Some (Aws.Query.Pair ("AliasName", String.to_query v.alias_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TargetKeyId", String.to_json v.target_key_id)
         ; Some ("AliasName", String.to_json v.alias_name)
         ])

  let of_json j =
    { alias_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AliasName"))
    ; target_key_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetKeyId"))
    }
end

module UnsupportedOperationException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DataKeySpec = struct
  type t =
    | AES_256
    | AES_128

  let str_to_t = [ "AES_128", AES_128; "AES_256", AES_256 ]

  let t_to_str = [ AES_128, "AES_128"; AES_256, "AES_256" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; encryption_context =
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; key_spec = Aws.Util.option_bind (Aws.Xml.member "KeySpec" xml) DataKeySpec.parse
      ; number_of_bytes =
          Aws.Util.option_bind (Aws.Xml.member "NumberOfBytes" xml) Integer.parse
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Aws.Util.option_map v.number_of_bytes (fun f ->
               Aws.Query.Pair ("NumberOfBytes", Integer.to_query f))
         ; Aws.Util.option_map v.key_spec (fun f ->
               Aws.Query.Pair ("KeySpec", DataKeySpec.to_query f))
         ; Aws.Util.option_map v.encryption_context (fun f ->
               Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Aws.Util.option_map v.number_of_bytes (fun f ->
               "NumberOfBytes", Integer.to_json f)
         ; Aws.Util.option_map v.key_spec (fun f -> "KeySpec", DataKeySpec.to_json f)
         ; Aws.Util.option_map v.encryption_context (fun f ->
               "EncryptionContext", EncryptionContextType.to_json f)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; encryption_context =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionContext")
          EncryptionContextType.of_json
    ; key_spec = Aws.Util.option_map (Aws.Json.lookup j "KeySpec") DataKeySpec.of_json
    ; number_of_bytes =
        Aws.Util.option_map (Aws.Json.lookup j "NumberOfBytes") Integer.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; grantee_principal =
          Aws.Xml.required
            "GranteePrincipal"
            (Aws.Util.option_bind (Aws.Xml.member "GranteePrincipal" xml) String.parse)
      ; retiring_principal =
          Aws.Util.option_bind (Aws.Xml.member "RetiringPrincipal" xml) String.parse
      ; operations =
          Aws.Xml.required
            "Operations"
            (Aws.Util.option_bind
               (Aws.Xml.member "Operations" xml)
               GrantOperationList.parse)
      ; constraints =
          Aws.Util.option_bind (Aws.Xml.member "Constraints" xml) GrantConstraints.parse
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ; Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Aws.Util.option_map v.constraints (fun f ->
               Aws.Query.Pair ("Constraints", GrantConstraints.to_query f))
         ; Some
             (Aws.Query.Pair
                ("Operations.member", GrantOperationList.to_query v.operations))
         ; Aws.Util.option_map v.retiring_principal (fun f ->
               Aws.Query.Pair ("RetiringPrincipal", String.to_query f))
         ; Some (Aws.Query.Pair ("GranteePrincipal", String.to_query v.grantee_principal))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Aws.Util.option_map v.constraints (fun f ->
               "Constraints", GrantConstraints.to_json f)
         ; Some ("Operations", GrantOperationList.to_json v.operations)
         ; Aws.Util.option_map v.retiring_principal (fun f ->
               "RetiringPrincipal", String.to_json f)
         ; Some ("GranteePrincipal", String.to_json v.grantee_principal)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; grantee_principal =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GranteePrincipal"))
    ; retiring_principal =
        Aws.Util.option_map (Aws.Json.lookup j "RetiringPrincipal") String.of_json
    ; operations =
        GrantOperationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Operations"))
    ; constraints =
        Aws.Util.option_map (Aws.Json.lookup j "Constraints") GrantConstraints.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    }
end

module InvalidKeyUsageException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
          Aws.Xml.required
            "CiphertextBlob"
            (Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse)
      ; plaintext =
          Aws.Xml.required
            "Plaintext"
            (Aws.Util.option_bind (Aws.Xml.member "Plaintext" xml) Blob.parse)
      ; key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ; Some (Aws.Query.Pair ("Plaintext", Blob.to_query v.plaintext))
         ; Some (Aws.Query.Pair ("CiphertextBlob", Blob.to_query v.ciphertext_blob))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("KeyId", String.to_json v.key_id)
         ; Some ("Plaintext", Blob.to_json v.plaintext)
         ; Some ("CiphertextBlob", Blob.to_json v.ciphertext_blob)
         ])

  let of_json j =
    { ciphertext_blob =
        Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CiphertextBlob"))
    ; plaintext = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Plaintext"))
    ; key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; primary_region =
          Aws.Xml.required
            "PrimaryRegion"
            (Aws.Util.option_bind (Aws.Xml.member "PrimaryRegion" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("PrimaryRegion", String.to_query v.primary_region))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PrimaryRegion", String.to_json v.primary_region)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; primary_region =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrimaryRegion"))
    }
end

module ListKeysRequest = struct
  type t =
    { limit : Integer.t option
    ; marker : String.t option
    }

  let make ?limit ?marker () = { limit; marker }

  let parse xml =
    Some
      { limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.limit (fun f ->
               Aws.Query.Pair ("Limit", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
         ])

  let of_json j =
    { limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module CloudHsmClusterInvalidConfigurationException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; policy_name =
          Aws.Xml.required
            "PolicyName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyName", String.to_json v.policy_name)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; policy_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
    }
end

module DisabledException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
          Aws.Xml.required
            "CiphertextBlob"
            (Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse)
      ; encryption_context =
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; encryption_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_algorithm (fun f ->
               Aws.Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ; Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Aws.Util.option_map v.encryption_context (fun f ->
               Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ; Some (Aws.Query.Pair ("CiphertextBlob", Blob.to_query v.ciphertext_blob))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_algorithm (fun f ->
               "EncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Aws.Util.option_map v.encryption_context (fun f ->
               "EncryptionContext", EncryptionContextType.to_json f)
         ; Some ("CiphertextBlob", Blob.to_json v.ciphertext_blob)
         ])

  let of_json j =
    { ciphertext_blob =
        Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CiphertextBlob"))
    ; encryption_context =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionContext")
          EncryptionContextType.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; encryption_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionAlgorithm")
          EncryptionAlgorithmSpec.of_json
    }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; policy_name =
          Aws.Xml.required
            "PolicyName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
      ; policy =
          Aws.Xml.required
            "Policy"
            (Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse)
      ; bypass_policy_lockout_safety_check =
          Aws.Util.option_bind
            (Aws.Xml.member "BypassPolicyLockoutSafetyCheck" xml)
            Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               Aws.Query.Pair ("BypassPolicyLockoutSafetyCheck", Boolean.to_query f))
         ; Some (Aws.Query.Pair ("Policy", String.to_query v.policy))
         ; Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.bypass_policy_lockout_safety_check (fun f ->
               "BypassPolicyLockoutSafetyCheck", Boolean.to_json f)
         ; Some ("Policy", String.to_json v.policy)
         ; Some ("PolicyName", String.to_json v.policy_name)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; policy_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
    ; policy = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Policy"))
    ; bypass_policy_lockout_safety_check =
        Aws.Util.option_map
          (Aws.Json.lookup j "BypassPolicyLockoutSafetyCheck")
          Boolean.of_json
    }
end

module DeleteAliasRequest = struct
  type t = { alias_name : String.t }

  let make ~alias_name () = { alias_name }

  let parse xml =
    Some
      { alias_name =
          Aws.Xml.required
            "AliasName"
            (Aws.Util.option_bind (Aws.Xml.member "AliasName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("AliasName", String.to_query v.alias_name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("AliasName", String.to_json v.alias_name) ])

  let of_json j =
    { alias_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AliasName"))
    }
end

module CreateGrantResponse = struct
  type t =
    { grant_token : String.t option
    ; grant_id : String.t option
    }

  let make ?grant_token ?grant_id () = { grant_token; grant_id }

  let parse xml =
    Some
      { grant_token = Aws.Util.option_bind (Aws.Xml.member "GrantToken" xml) String.parse
      ; grant_id = Aws.Util.option_bind (Aws.Xml.member "GrantId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.grant_id (fun f ->
               Aws.Query.Pair ("GrantId", String.to_query f))
         ; Aws.Util.option_map v.grant_token (fun f ->
               Aws.Query.Pair ("GrantToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.grant_id (fun f -> "GrantId", String.to_json f)
         ; Aws.Util.option_map v.grant_token (fun f -> "GrantToken", String.to_json f)
         ])

  let of_json j =
    { grant_token = Aws.Util.option_map (Aws.Json.lookup j "GrantToken") String.of_json
    ; grant_id = Aws.Util.option_map (Aws.Json.lookup j "GrantId") String.of_json
    }
end

module IncorrectKeyMaterialException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module NotFoundException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; key_pair_spec =
          Aws.Xml.required
            "KeyPairSpec"
            (Aws.Util.option_bind
               (Aws.Xml.member "KeyPairSpec" xml)
               DataKeyPairSpec.parse)
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some (Aws.Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query v.key_pair_spec))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ; Aws.Util.option_map v.encryption_context (fun f ->
               Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("KeyPairSpec", DataKeyPairSpec.to_json v.key_pair_spec)
         ; Some ("KeyId", String.to_json v.key_id)
         ; Aws.Util.option_map v.encryption_context (fun f ->
               "EncryptionContext", EncryptionContextType.to_json f)
         ])

  let of_json j =
    { encryption_context =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionContext")
          EncryptionContextType.of_json
    ; key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; key_pair_spec =
        DataKeyPairSpec.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyPairSpec"))
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    }
end

module CloudHsmClusterNotFoundException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module PolicyNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.limit (fun f ->
               Aws.Query.Pair ("Limit", Integer.to_query f))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; message =
          Aws.Xml.required
            "Message"
            (Aws.Util.option_bind (Aws.Xml.member "Message" xml) Blob.parse)
      ; message_type =
          Aws.Util.option_bind (Aws.Xml.member "MessageType" xml) MessageType.parse
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      ; signing_algorithm =
          Aws.Xml.required
            "SigningAlgorithm"
            (Aws.Util.option_bind
               (Aws.Xml.member "SigningAlgorithm" xml)
               SigningAlgorithmSpec.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("SigningAlgorithm", SigningAlgorithmSpec.to_query v.signing_algorithm))
         ; Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Aws.Util.option_map v.message_type (fun f ->
               Aws.Query.Pair ("MessageType", MessageType.to_query f))
         ; Some (Aws.Query.Pair ("Message", Blob.to_query v.message))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("SigningAlgorithm", SigningAlgorithmSpec.to_json v.signing_algorithm)
         ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Aws.Util.option_map v.message_type (fun f ->
               "MessageType", MessageType.to_json f)
         ; Some ("Message", Blob.to_json v.message)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; message = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Message"))
    ; message_type =
        Aws.Util.option_map (Aws.Json.lookup j "MessageType") MessageType.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    ; signing_algorithm =
        SigningAlgorithmSpec.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SigningAlgorithm"))
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
      { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.limit (fun f ->
               Aws.Query.Pair ("Limit", Integer.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
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
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Grants" xml) GrantList.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      ; truncated = Aws.Util.option_bind (Aws.Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f ->
               Aws.Query.Pair ("Truncated", Boolean.to_query f))
         ; Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some (Aws.Query.Pair ("Grants.member", GrantList.to_query v.grants))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
         ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some ("Grants", GrantList.to_json v.grants)
         ])

  let of_json j =
    { grants = GrantList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Grants"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
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
      { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; signature_valid =
          Aws.Util.option_bind (Aws.Xml.member "SignatureValid" xml) Boolean.parse
      ; signing_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "SigningAlgorithm" xml)
            SigningAlgorithmSpec.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.signing_algorithm (fun f ->
               Aws.Query.Pair ("SigningAlgorithm", SigningAlgorithmSpec.to_query f))
         ; Aws.Util.option_map v.signature_valid (fun f ->
               Aws.Query.Pair ("SignatureValid", Boolean.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.signing_algorithm (fun f ->
               "SigningAlgorithm", SigningAlgorithmSpec.to_json f)
         ; Aws.Util.option_map v.signature_valid (fun f ->
               "SignatureValid", Boolean.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; signature_valid =
        Aws.Util.option_map (Aws.Json.lookup j "SignatureValid") Boolean.of_json
    ; signing_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "SigningAlgorithm")
          SigningAlgorithmSpec.of_json
    }
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
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNameList.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      ; truncated = Aws.Util.option_bind (Aws.Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f ->
               Aws.Query.Pair ("Truncated", Boolean.to_query f))
         ; Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some
             (Aws.Query.Pair ("PolicyNames.member", PolicyNameList.to_query v.policy_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
         ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some ("PolicyNames", PolicyNameList.to_json v.policy_names)
         ])

  let of_json j =
    { policy_names =
        PolicyNameList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
    }
end

module InvalidGrantTokenException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
          Aws.Xml.required
            "AliasName"
            (Aws.Util.option_bind (Aws.Xml.member "AliasName" xml) String.parse)
      ; target_key_id =
          Aws.Xml.required
            "TargetKeyId"
            (Aws.Util.option_bind (Aws.Xml.member "TargetKeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TargetKeyId", String.to_query v.target_key_id))
         ; Some (Aws.Query.Pair ("AliasName", String.to_query v.alias_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TargetKeyId", String.to_json v.target_key_id)
         ; Some ("AliasName", String.to_json v.alias_name)
         ])

  let of_json j =
    { alias_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AliasName"))
    ; target_key_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetKeyId"))
    }
end

module AlreadyExistsException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
      { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; signature = Aws.Util.option_bind (Aws.Xml.member "Signature" xml) Blob.parse
      ; signing_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "SigningAlgorithm" xml)
            SigningAlgorithmSpec.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.signing_algorithm (fun f ->
               Aws.Query.Pair ("SigningAlgorithm", SigningAlgorithmSpec.to_query f))
         ; Aws.Util.option_map v.signature (fun f ->
               Aws.Query.Pair ("Signature", Blob.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.signing_algorithm (fun f ->
               "SigningAlgorithm", SigningAlgorithmSpec.to_json f)
         ; Aws.Util.option_map v.signature (fun f -> "Signature", Blob.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; signature = Aws.Util.option_map (Aws.Json.lookup j "Signature") Blob.of_json
    ; signing_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "SigningAlgorithm")
          SigningAlgorithmSpec.of_json
    }
end

module KMSInternalException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
      { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; plaintext = Aws.Util.option_bind (Aws.Xml.member "Plaintext" xml) Blob.parse
      ; encryption_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_algorithm (fun f ->
               Aws.Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Aws.Util.option_map v.plaintext (fun f ->
               Aws.Query.Pair ("Plaintext", Blob.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_algorithm (fun f ->
               "EncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
         ; Aws.Util.option_map v.plaintext (fun f -> "Plaintext", Blob.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; plaintext = Aws.Util.option_map (Aws.Json.lookup j "Plaintext") Blob.of_json
    ; encryption_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionAlgorithm")
          EncryptionAlgorithmSpec.of_json
    }
end

module DeleteCustomKeyStoreResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.limit (fun f ->
               Aws.Query.Pair ("Limit", Integer.to_query f))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module CloudHsmClusterNotRelatedException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
      { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; public_key = Aws.Util.option_bind (Aws.Xml.member "PublicKey" xml) Blob.parse
      ; customer_master_key_spec =
          Aws.Util.option_bind
            (Aws.Xml.member "CustomerMasterKeySpec" xml)
            CustomerMasterKeySpec.parse
      ; key_spec = Aws.Util.option_bind (Aws.Xml.member "KeySpec" xml) KeySpec.parse
      ; key_usage =
          Aws.Util.option_bind (Aws.Xml.member "KeyUsage" xml) KeyUsageType.parse
      ; encryption_algorithms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "EncryptionAlgorithms" xml)
               EncryptionAlgorithmSpecList.parse)
      ; signing_algorithms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SigningAlgorithms" xml)
               SigningAlgorithmSpecList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "SigningAlgorithms.member"
                , SigningAlgorithmSpecList.to_query v.signing_algorithms ))
         ; Some
             (Aws.Query.Pair
                ( "EncryptionAlgorithms.member"
                , EncryptionAlgorithmSpecList.to_query v.encryption_algorithms ))
         ; Aws.Util.option_map v.key_usage (fun f ->
               Aws.Query.Pair ("KeyUsage", KeyUsageType.to_query f))
         ; Aws.Util.option_map v.key_spec (fun f ->
               Aws.Query.Pair ("KeySpec", KeySpec.to_query f))
         ; Aws.Util.option_map v.customer_master_key_spec (fun f ->
               Aws.Query.Pair ("CustomerMasterKeySpec", CustomerMasterKeySpec.to_query f))
         ; Aws.Util.option_map v.public_key (fun f ->
               Aws.Query.Pair ("PublicKey", Blob.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("SigningAlgorithms", SigningAlgorithmSpecList.to_json v.signing_algorithms)
         ; Some
             ( "EncryptionAlgorithms"
             , EncryptionAlgorithmSpecList.to_json v.encryption_algorithms )
         ; Aws.Util.option_map v.key_usage (fun f -> "KeyUsage", KeyUsageType.to_json f)
         ; Aws.Util.option_map v.key_spec (fun f -> "KeySpec", KeySpec.to_json f)
         ; Aws.Util.option_map v.customer_master_key_spec (fun f ->
               "CustomerMasterKeySpec", CustomerMasterKeySpec.to_json f)
         ; Aws.Util.option_map v.public_key (fun f -> "PublicKey", Blob.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ])

  let of_json j =
    { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; public_key = Aws.Util.option_map (Aws.Json.lookup j "PublicKey") Blob.of_json
    ; customer_master_key_spec =
        Aws.Util.option_map
          (Aws.Json.lookup j "CustomerMasterKeySpec")
          CustomerMasterKeySpec.of_json
    ; key_spec = Aws.Util.option_map (Aws.Json.lookup j "KeySpec") KeySpec.of_json
    ; key_usage = Aws.Util.option_map (Aws.Json.lookup j "KeyUsage") KeyUsageType.of_json
    ; encryption_algorithms =
        EncryptionAlgorithmSpecList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EncryptionAlgorithms"))
    ; signing_algorithms =
        SigningAlgorithmSpecList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SigningAlgorithms"))
    }
end

module IncorrectKeyException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DeleteImportedKeyMaterialRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("KeyId", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId")) }
end

module DeleteCustomKeyStoreRequest = struct
  type t = { custom_key_store_id : String.t }

  let make ~custom_key_store_id () = { custom_key_store_id }

  let parse xml =
    Some
      { custom_key_store_id =
          Aws.Xml.required
            "CustomKeyStoreId"
            (Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CustomKeyStoreId", String.to_json v.custom_key_store_id) ])

  let of_json j =
    { custom_key_store_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomKeyStoreId"))
    }
end

module EnableKeyRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("KeyId", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId")) }
end

module InvalidMarkerException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module CustomKeyStoreNameInUseException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
          Aws.Xml.required
            "CiphertextBlob"
            (Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse)
      ; source_encryption_context =
          Aws.Util.option_bind
            (Aws.Xml.member "SourceEncryptionContext" xml)
            EncryptionContextType.parse
      ; source_key_id =
          Aws.Util.option_bind (Aws.Xml.member "SourceKeyId" xml) String.parse
      ; destination_key_id =
          Aws.Xml.required
            "DestinationKeyId"
            (Aws.Util.option_bind (Aws.Xml.member "DestinationKeyId" xml) String.parse)
      ; destination_encryption_context =
          Aws.Util.option_bind
            (Aws.Xml.member "DestinationEncryptionContext" xml)
            EncryptionContextType.parse
      ; source_encryption_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "SourceEncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      ; destination_encryption_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "DestinationEncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Aws.Util.option_map v.destination_encryption_algorithm (fun f ->
               Aws.Query.Pair
                 ("DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Aws.Util.option_map v.source_encryption_algorithm (fun f ->
               Aws.Query.Pair
                 ("SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Aws.Util.option_map v.destination_encryption_context (fun f ->
               Aws.Query.Pair
                 ("DestinationEncryptionContext", EncryptionContextType.to_query f))
         ; Some
             (Aws.Query.Pair ("DestinationKeyId", String.to_query v.destination_key_id))
         ; Aws.Util.option_map v.source_key_id (fun f ->
               Aws.Query.Pair ("SourceKeyId", String.to_query f))
         ; Aws.Util.option_map v.source_encryption_context (fun f ->
               Aws.Query.Pair ("SourceEncryptionContext", EncryptionContextType.to_query f))
         ; Some (Aws.Query.Pair ("CiphertextBlob", Blob.to_query v.ciphertext_blob))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Aws.Util.option_map v.destination_encryption_algorithm (fun f ->
               "DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
         ; Aws.Util.option_map v.source_encryption_algorithm (fun f ->
               "SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
         ; Aws.Util.option_map v.destination_encryption_context (fun f ->
               "DestinationEncryptionContext", EncryptionContextType.to_json f)
         ; Some ("DestinationKeyId", String.to_json v.destination_key_id)
         ; Aws.Util.option_map v.source_key_id (fun f -> "SourceKeyId", String.to_json f)
         ; Aws.Util.option_map v.source_encryption_context (fun f ->
               "SourceEncryptionContext", EncryptionContextType.to_json f)
         ; Some ("CiphertextBlob", Blob.to_json v.ciphertext_blob)
         ])

  let of_json j =
    { ciphertext_blob =
        Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CiphertextBlob"))
    ; source_encryption_context =
        Aws.Util.option_map
          (Aws.Json.lookup j "SourceEncryptionContext")
          EncryptionContextType.of_json
    ; source_key_id = Aws.Util.option_map (Aws.Json.lookup j "SourceKeyId") String.of_json
    ; destination_key_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationKeyId"))
    ; destination_encryption_context =
        Aws.Util.option_map
          (Aws.Json.lookup j "DestinationEncryptionContext")
          EncryptionContextType.of_json
    ; source_encryption_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "SourceEncryptionAlgorithm")
          EncryptionAlgorithmSpec.of_json
    ; destination_encryption_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "DestinationEncryptionAlgorithm")
          EncryptionAlgorithmSpec.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    }
end

module GetKeyRotationStatusRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("KeyId", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId")) }
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
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; key_pair_spec =
          Aws.Xml.required
            "KeyPairSpec"
            (Aws.Util.option_bind
               (Aws.Xml.member "KeyPairSpec" xml)
               DataKeyPairSpec.parse)
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some (Aws.Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query v.key_pair_spec))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ; Aws.Util.option_map v.encryption_context (fun f ->
               Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("KeyPairSpec", DataKeyPairSpec.to_json v.key_pair_spec)
         ; Some ("KeyId", String.to_json v.key_id)
         ; Aws.Util.option_map v.encryption_context (fun f ->
               "EncryptionContext", EncryptionContextType.to_json f)
         ])

  let of_json j =
    { encryption_context =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionContext")
          EncryptionContextType.of_json
    ; key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; key_pair_spec =
        DataKeyPairSpec.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyPairSpec"))
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    }
end

module CustomKeyStoresList = struct
  type t = CustomKeyStoresListEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map CustomKeyStoresListEntry.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list CustomKeyStoresListEntry.to_query v

  let to_json v = `List (List.map CustomKeyStoresListEntry.to_json v)

  let of_json j = Aws.Json.to_list CustomKeyStoresListEntry.of_json j
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; pending_window_in_days =
          Aws.Util.option_bind (Aws.Xml.member "PendingWindowInDays" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.pending_window_in_days (fun f ->
               Aws.Query.Pair ("PendingWindowInDays", Integer.to_query f))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.pending_window_in_days (fun f ->
               "PendingWindowInDays", Integer.to_json f)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; pending_window_in_days =
        Aws.Util.option_map (Aws.Json.lookup j "PendingWindowInDays") Integer.of_json
    }
end

module MalformedPolicyDocumentException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module KeyUnavailableException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module EnableKeyRotationRequest = struct
  type t = { key_id : String.t }

  let make ~key_id () = { key_id }

  let parse xml =
    Some
      { key_id =
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("KeyId", String.to_json v.key_id) ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId")) }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; import_token =
          Aws.Xml.required
            "ImportToken"
            (Aws.Util.option_bind (Aws.Xml.member "ImportToken" xml) Blob.parse)
      ; encrypted_key_material =
          Aws.Xml.required
            "EncryptedKeyMaterial"
            (Aws.Util.option_bind (Aws.Xml.member "EncryptedKeyMaterial" xml) Blob.parse)
      ; valid_to = Aws.Util.option_bind (Aws.Xml.member "ValidTo" xml) DateTime.parse
      ; expiration_model =
          Aws.Util.option_bind
            (Aws.Xml.member "ExpirationModel" xml)
            ExpirationModelType.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.expiration_model (fun f ->
               Aws.Query.Pair ("ExpirationModel", ExpirationModelType.to_query f))
         ; Aws.Util.option_map v.valid_to (fun f ->
               Aws.Query.Pair ("ValidTo", DateTime.to_query f))
         ; Some
             (Aws.Query.Pair
                ("EncryptedKeyMaterial", Blob.to_query v.encrypted_key_material))
         ; Some (Aws.Query.Pair ("ImportToken", Blob.to_query v.import_token))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.expiration_model (fun f ->
               "ExpirationModel", ExpirationModelType.to_json f)
         ; Aws.Util.option_map v.valid_to (fun f -> "ValidTo", DateTime.to_json f)
         ; Some ("EncryptedKeyMaterial", Blob.to_json v.encrypted_key_material)
         ; Some ("ImportToken", Blob.to_json v.import_token)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; import_token =
        Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImportToken"))
    ; encrypted_key_material =
        Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EncryptedKeyMaterial"))
    ; valid_to = Aws.Util.option_map (Aws.Json.lookup j "ValidTo") DateTime.of_json
    ; expiration_model =
        Aws.Util.option_map
          (Aws.Json.lookup j "ExpirationModel")
          ExpirationModelType.of_json
    }
end

module CustomKeyStoreInvalidStateException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module InvalidArnException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module CustomKeyStoreNotFoundException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DependencyTimeoutException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
      { number_of_bytes =
          Aws.Util.option_bind (Aws.Xml.member "NumberOfBytes" xml) Integer.parse
      ; custom_key_store_id =
          Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.custom_key_store_id (fun f ->
               Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
         ; Aws.Util.option_map v.number_of_bytes (fun f ->
               Aws.Query.Pair ("NumberOfBytes", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.custom_key_store_id (fun f ->
               "CustomKeyStoreId", String.to_json f)
         ; Aws.Util.option_map v.number_of_bytes (fun f ->
               "NumberOfBytes", Integer.to_json f)
         ])

  let of_json j =
    { number_of_bytes =
        Aws.Util.option_map (Aws.Json.lookup j "NumberOfBytes") Integer.of_json
    ; custom_key_store_id =
        Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
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
      { ciphertext_blob =
          Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse
      ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; encryption_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_algorithm (fun f ->
               Aws.Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ; Aws.Util.option_map v.ciphertext_blob (fun f ->
               Aws.Query.Pair ("CiphertextBlob", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_algorithm (fun f ->
               "EncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ; Aws.Util.option_map v.ciphertext_blob (fun f ->
               "CiphertextBlob", Blob.to_json f)
         ])

  let of_json j =
    { ciphertext_blob =
        Aws.Util.option_map (Aws.Json.lookup j "CiphertextBlob") Blob.of_json
    ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; encryption_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionAlgorithm")
          EncryptionAlgorithmSpec.of_json
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
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CustomKeyStores" xml)
               CustomKeyStoresList.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      ; truncated = Aws.Util.option_bind (Aws.Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f ->
               Aws.Query.Pair ("Truncated", Boolean.to_query f))
         ; Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "CustomKeyStores.member"
                , CustomKeyStoresList.to_query v.custom_key_stores ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
         ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some ("CustomKeyStores", CustomKeyStoresList.to_json v.custom_key_stores)
         ])

  let of_json j =
    { custom_key_stores =
        CustomKeyStoresList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomKeyStores"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
    }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; encryption_context =
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; number_of_bytes =
          Aws.Util.option_bind (Aws.Xml.member "NumberOfBytes" xml) Integer.parse
      ; key_spec = Aws.Util.option_bind (Aws.Xml.member "KeySpec" xml) DataKeySpec.parse
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Aws.Util.option_map v.key_spec (fun f ->
               Aws.Query.Pair ("KeySpec", DataKeySpec.to_query f))
         ; Aws.Util.option_map v.number_of_bytes (fun f ->
               Aws.Query.Pair ("NumberOfBytes", Integer.to_query f))
         ; Aws.Util.option_map v.encryption_context (fun f ->
               Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Aws.Util.option_map v.key_spec (fun f -> "KeySpec", DataKeySpec.to_json f)
         ; Aws.Util.option_map v.number_of_bytes (fun f ->
               "NumberOfBytes", Integer.to_json f)
         ; Aws.Util.option_map v.encryption_context (fun f ->
               "EncryptionContext", EncryptionContextType.to_json f)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; encryption_context =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionContext")
          EncryptionContextType.of_json
    ; number_of_bytes =
        Aws.Util.option_map (Aws.Json.lookup j "NumberOfBytes") Integer.of_json
    ; key_spec = Aws.Util.option_map (Aws.Json.lookup j "KeySpec") DataKeySpec.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    }
end

module GenerateRandomResponse = struct
  type t = { plaintext : Blob.t option }

  let make ?plaintext () = { plaintext }

  let parse xml =
    Some { plaintext = Aws.Util.option_bind (Aws.Xml.member "Plaintext" xml) Blob.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.plaintext (fun f ->
               Aws.Query.Pair ("Plaintext", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.plaintext (fun f -> "Plaintext", Blob.to_json f) ])

  let of_json j =
    { plaintext = Aws.Util.option_map (Aws.Json.lookup j "Plaintext") Blob.of_json }
end

module ExpiredImportTokenException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module CreateCustomKeyStoreResponse = struct
  type t = { custom_key_store_id : String.t option }

  let make ?custom_key_store_id () = { custom_key_store_id }

  let parse xml =
    Some
      { custom_key_store_id =
          Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.custom_key_store_id (fun f ->
               Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.custom_key_store_id (fun f ->
               "CustomKeyStoreId", String.to_json f)
         ])

  let of_json j =
    { custom_key_store_id =
        Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
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
      { grant_token = Aws.Util.option_bind (Aws.Xml.member "GrantToken" xml) String.parse
      ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
      ; grant_id = Aws.Util.option_bind (Aws.Xml.member "GrantId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.grant_id (fun f ->
               Aws.Query.Pair ("GrantId", String.to_query f))
         ; Aws.Util.option_map v.key_id (fun f ->
               Aws.Query.Pair ("KeyId", String.to_query f))
         ; Aws.Util.option_map v.grant_token (fun f ->
               Aws.Query.Pair ("GrantToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.grant_id (fun f -> "GrantId", String.to_json f)
         ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
         ; Aws.Util.option_map v.grant_token (fun f -> "GrantToken", String.to_json f)
         ])

  let of_json j =
    { grant_token = Aws.Util.option_map (Aws.Json.lookup j "GrantToken") String.of_json
    ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
    ; grant_id = Aws.Util.option_map (Aws.Json.lookup j "GrantId") String.of_json
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
      { limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; retiring_principal =
          Aws.Xml.required
            "RetiringPrincipal"
            (Aws.Util.option_bind (Aws.Xml.member "RetiringPrincipal" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("RetiringPrincipal", String.to_query v.retiring_principal))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.limit (fun f ->
               Aws.Query.Pair ("Limit", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("RetiringPrincipal", String.to_json v.retiring_principal)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
         ])

  let of_json j =
    { limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; retiring_principal =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RetiringPrincipal"))
    }
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
          Aws.Xml.required
            "CustomKeyStoreId"
            (Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse)
      ; new_custom_key_store_name =
          Aws.Util.option_bind (Aws.Xml.member "NewCustomKeyStoreName" xml) String.parse
      ; key_store_password =
          Aws.Util.option_bind (Aws.Xml.member "KeyStorePassword" xml) String.parse
      ; cloud_hsm_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CloudHsmClusterId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cloud_hsm_cluster_id (fun f ->
               Aws.Query.Pair ("CloudHsmClusterId", String.to_query f))
         ; Aws.Util.option_map v.key_store_password (fun f ->
               Aws.Query.Pair ("KeyStorePassword", String.to_query f))
         ; Aws.Util.option_map v.new_custom_key_store_name (fun f ->
               Aws.Query.Pair ("NewCustomKeyStoreName", String.to_query f))
         ; Some
             (Aws.Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cloud_hsm_cluster_id (fun f ->
               "CloudHsmClusterId", String.to_json f)
         ; Aws.Util.option_map v.key_store_password (fun f ->
               "KeyStorePassword", String.to_json f)
         ; Aws.Util.option_map v.new_custom_key_store_name (fun f ->
               "NewCustomKeyStoreName", String.to_json f)
         ; Some ("CustomKeyStoreId", String.to_json v.custom_key_store_id)
         ])

  let of_json j =
    { custom_key_store_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomKeyStoreId"))
    ; new_custom_key_store_name =
        Aws.Util.option_map (Aws.Json.lookup j "NewCustomKeyStoreName") String.of_json
    ; key_store_password =
        Aws.Util.option_map (Aws.Json.lookup j "KeyStorePassword") String.of_json
    ; cloud_hsm_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CloudHsmClusterId") String.of_json
    }
end

module KMSInvalidSignatureException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; plaintext =
          Aws.Xml.required
            "Plaintext"
            (Aws.Util.option_bind (Aws.Xml.member "Plaintext" xml) Blob.parse)
      ; encryption_context =
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionContext" xml)
            EncryptionContextType.parse
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      ; encryption_algorithm =
          Aws.Util.option_bind
            (Aws.Xml.member "EncryptionAlgorithm" xml)
            EncryptionAlgorithmSpec.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_algorithm (fun f ->
               Aws.Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
         ; Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Aws.Util.option_map v.encryption_context (fun f ->
               Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
         ; Some (Aws.Query.Pair ("Plaintext", Blob.to_query v.plaintext))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.encryption_algorithm (fun f ->
               "EncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
         ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Aws.Util.option_map v.encryption_context (fun f ->
               "EncryptionContext", EncryptionContextType.to_json f)
         ; Some ("Plaintext", Blob.to_json v.plaintext)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; plaintext = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Plaintext"))
    ; encryption_context =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionContext")
          EncryptionContextType.of_json
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    ; encryption_algorithm =
        Aws.Util.option_map
          (Aws.Json.lookup j "EncryptionAlgorithm")
          EncryptionAlgorithmSpec.of_json
    }
end

module DescribeKeyResponse = struct
  type t = { key_metadata : KeyMetadata.t option }

  let make ?key_metadata () = { key_metadata }

  let parse xml =
    Some
      { key_metadata =
          Aws.Util.option_bind (Aws.Xml.member "KeyMetadata" xml) KeyMetadata.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_metadata (fun f ->
               Aws.Query.Pair ("KeyMetadata", KeyMetadata.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key_metadata (fun f ->
               "KeyMetadata", KeyMetadata.to_json f)
         ])

  let of_json j =
    { key_metadata =
        Aws.Util.option_map (Aws.Json.lookup j "KeyMetadata") KeyMetadata.of_json
    }
end

module CloudHsmClusterNotActiveException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module KMSInvalidStateException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; message =
          Aws.Xml.required
            "Message"
            (Aws.Util.option_bind (Aws.Xml.member "Message" xml) Blob.parse)
      ; message_type =
          Aws.Util.option_bind (Aws.Xml.member "MessageType" xml) MessageType.parse
      ; signature =
          Aws.Xml.required
            "Signature"
            (Aws.Util.option_bind (Aws.Xml.member "Signature" xml) Blob.parse)
      ; signing_algorithm =
          Aws.Xml.required
            "SigningAlgorithm"
            (Aws.Util.option_bind
               (Aws.Xml.member "SigningAlgorithm" xml)
               SigningAlgorithmSpec.parse)
      ; grant_tokens =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
         ; Some
             (Aws.Query.Pair
                ("SigningAlgorithm", SigningAlgorithmSpec.to_query v.signing_algorithm))
         ; Some (Aws.Query.Pair ("Signature", Blob.to_query v.signature))
         ; Aws.Util.option_map v.message_type (fun f ->
               Aws.Query.Pair ("MessageType", MessageType.to_query f))
         ; Some (Aws.Query.Pair ("Message", Blob.to_query v.message))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
         ; Some ("SigningAlgorithm", SigningAlgorithmSpec.to_json v.signing_algorithm)
         ; Some ("Signature", Blob.to_json v.signature)
         ; Aws.Util.option_map v.message_type (fun f ->
               "MessageType", MessageType.to_json f)
         ; Some ("Message", Blob.to_json v.message)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; message = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Message"))
    ; message_type =
        Aws.Util.option_map (Aws.Json.lookup j "MessageType") MessageType.of_json
    ; signature = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Signature"))
    ; signing_algorithm =
        SigningAlgorithmSpec.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SigningAlgorithm"))
    ; grant_tokens =
        GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
    }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; tags =
          Aws.Xml.required
            "Tags"
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module ImportKeyMaterialResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
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
      { keys =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Keys" xml) KeyList.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      ; truncated = Aws.Util.option_bind (Aws.Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f ->
               Aws.Query.Pair ("Truncated", Boolean.to_query f))
         ; Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some (Aws.Query.Pair ("Keys.member", KeyList.to_query v.keys))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
         ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some ("Keys", KeyList.to_json v.keys)
         ])

  let of_json j =
    { keys = KeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Keys"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
    }
end

module InvalidCiphertextException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module GetKeyPolicyResponse = struct
  type t = { policy : String.t option }

  let make ?policy () = { policy }

  let parse xml =
    Some { policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.policy (fun f ->
               Aws.Query.Pair ("Policy", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f) ])

  let of_json j =
    { policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json }
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
          Aws.Xml.required
            "KeyId"
            (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
      ; wrapping_algorithm =
          Aws.Xml.required
            "WrappingAlgorithm"
            (Aws.Util.option_bind
               (Aws.Xml.member "WrappingAlgorithm" xml)
               AlgorithmSpec.parse)
      ; wrapping_key_spec =
          Aws.Xml.required
            "WrappingKeySpec"
            (Aws.Util.option_bind
               (Aws.Xml.member "WrappingKeySpec" xml)
               WrappingKeySpec.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("WrappingKeySpec", WrappingKeySpec.to_query v.wrapping_key_spec))
         ; Some
             (Aws.Query.Pair
                ("WrappingAlgorithm", AlgorithmSpec.to_query v.wrapping_algorithm))
         ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("WrappingKeySpec", WrappingKeySpec.to_json v.wrapping_key_spec)
         ; Some ("WrappingAlgorithm", AlgorithmSpec.to_json v.wrapping_algorithm)
         ; Some ("KeyId", String.to_json v.key_id)
         ])

  let of_json j =
    { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
    ; wrapping_algorithm =
        AlgorithmSpec.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "WrappingAlgorithm"))
    ; wrapping_key_spec =
        WrappingKeySpec.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "WrappingKeySpec"))
    }
end

module DisconnectCustomKeyStoreResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CustomKeyStoreHasCMKsException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module CloudHsmClusterInUseException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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
      { tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      ; truncated = Aws.Util.option_bind (Aws.Xml.member "Truncated" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f ->
               Aws.Query.Pair ("Truncated", Boolean.to_query f))
         ; Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.truncated (fun f -> "Truncated", Boolean.to_json f)
         ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some ("Tags", TagList.to_json v.tags)
         ])

  let of_json j =
    { tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    ; truncated = Aws.Util.option_map (Aws.Json.lookup j "Truncated") Boolean.of_json
    }
end

module ConnectCustomKeyStoreRequest = struct
  type t = { custom_key_store_id : String.t }

  let make ~custom_key_store_id () = { custom_key_store_id }

  let parse xml =
    Some
      { custom_key_store_id =
          Aws.Xml.required
            "CustomKeyStoreId"
            (Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CustomKeyStoreId", String.to_json v.custom_key_store_id) ])

  let of_json j =
    { custom_key_store_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomKeyStoreId"))
    }
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
          Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
      ; custom_key_store_name =
          Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreName" xml) String.parse
      ; limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.limit (fun f ->
               Aws.Query.Pair ("Limit", Integer.to_query f))
         ; Aws.Util.option_map v.custom_key_store_name (fun f ->
               Aws.Query.Pair ("CustomKeyStoreName", String.to_query f))
         ; Aws.Util.option_map v.custom_key_store_id (fun f ->
               Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
         ; Aws.Util.option_map v.custom_key_store_name (fun f ->
               "CustomKeyStoreName", String.to_json f)
         ; Aws.Util.option_map v.custom_key_store_id (fun f ->
               "CustomKeyStoreId", String.to_json f)
         ])

  let of_json j =
    { custom_key_store_id =
        Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
    ; custom_key_store_name =
        Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreName") String.of_json
    ; limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module UpdateCustomKeyStoreResponse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidImportTokenException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end
