open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; public_key : Blob.t option
  ; customer_master_key_spec : CustomerMasterKeySpec.t option
  ; key_spec : KeySpec.t option
  ; key_usage : KeyUsageType.t option
  ; encryption_algorithms : EncryptionAlgorithmSpecList.t
  ; signing_algorithms : SigningAlgorithmSpecList.t
  ; key_agreement_algorithms : KeyAgreementAlgorithmSpecList.t
  }

let make
    ?key_id
    ?public_key
    ?customer_master_key_spec
    ?key_spec
    ?key_usage
    ?(encryption_algorithms = [])
    ?(signing_algorithms = [])
    ?(key_agreement_algorithms = [])
    () =
  { key_id
  ; public_key
  ; customer_master_key_spec
  ; key_spec
  ; key_usage
  ; encryption_algorithms
  ; signing_algorithms
  ; key_agreement_algorithms
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
    ; key_usage = Aws.Util.option_bind (Aws.Xml.member "KeyUsage" xml) KeyUsageType.parse
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
    ; key_agreement_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "KeyAgreementAlgorithms" xml)
             KeyAgreementAlgorithmSpecList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "KeyAgreementAlgorithms.member"
              , KeyAgreementAlgorithmSpecList.to_query v.key_agreement_algorithms ))
       ; Some
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
           ( "KeyAgreementAlgorithms"
           , KeyAgreementAlgorithmSpecList.to_json v.key_agreement_algorithms )
       ; Some ("SigningAlgorithms", SigningAlgorithmSpecList.to_json v.signing_algorithms)
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
  ; key_agreement_algorithms =
      KeyAgreementAlgorithmSpecList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyAgreementAlgorithms"))
  }
