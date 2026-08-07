open Aws.BaseTypes

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
  ; key_agreement_algorithms : KeyAgreementAlgorithmSpecList.t
  ; multi_region : Boolean.t option
  ; multi_region_configuration : MultiRegionConfiguration.t option
  ; pending_deletion_window_in_days : Integer.t option
  ; mac_algorithms : MacAlgorithmSpecList.t
  ; xks_key_configuration : XksKeyConfigurationType.t option
  ; current_key_material_id : String.t option
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
    ?(key_agreement_algorithms = [])
    ?multi_region
    ?multi_region_configuration
    ?pending_deletion_window_in_days
    ?(mac_algorithms = [])
    ?xks_key_configuration
    ?current_key_material_id
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
  ; key_agreement_algorithms
  ; multi_region
  ; multi_region_configuration
  ; pending_deletion_window_in_days
  ; mac_algorithms
  ; xks_key_configuration
  ; current_key_material_id
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
    ; key_usage = Aws.Util.option_bind (Aws.Xml.member "KeyUsage" xml) KeyUsageType.parse
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
    ; key_agreement_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "KeyAgreementAlgorithms" xml)
             KeyAgreementAlgorithmSpecList.parse)
    ; multi_region = Aws.Util.option_bind (Aws.Xml.member "MultiRegion" xml) Boolean.parse
    ; multi_region_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "MultiRegionConfiguration" xml)
          MultiRegionConfiguration.parse
    ; pending_deletion_window_in_days =
        Aws.Util.option_bind
          (Aws.Xml.member "PendingDeletionWindowInDays" xml)
          Integer.parse
    ; mac_algorithms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "MacAlgorithms" xml)
             MacAlgorithmSpecList.parse)
    ; xks_key_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "XksKeyConfiguration" xml)
          XksKeyConfigurationType.parse
    ; current_key_material_id =
        Aws.Util.option_bind (Aws.Xml.member "CurrentKeyMaterialId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.current_key_material_id (fun f ->
             Aws.Query.Pair ("CurrentKeyMaterialId", String.to_query f))
       ; Aws.Util.option_map v.xks_key_configuration (fun f ->
             Aws.Query.Pair ("XksKeyConfiguration", XksKeyConfigurationType.to_query f))
       ; Some
           (Aws.Query.Pair
              ("MacAlgorithms.member", MacAlgorithmSpecList.to_query v.mac_algorithms))
       ; Aws.Util.option_map v.pending_deletion_window_in_days (fun f ->
             Aws.Query.Pair ("PendingDeletionWindowInDays", Integer.to_query f))
       ; Aws.Util.option_map v.multi_region_configuration (fun f ->
             Aws.Query.Pair
               ("MultiRegionConfiguration", MultiRegionConfiguration.to_query f))
       ; Aws.Util.option_map v.multi_region (fun f ->
             Aws.Query.Pair ("MultiRegion", Boolean.to_query f))
       ; Some
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
       [ Aws.Util.option_map v.current_key_material_id (fun f ->
             "CurrentKeyMaterialId", String.to_json f)
       ; Aws.Util.option_map v.xks_key_configuration (fun f ->
             "XksKeyConfiguration", XksKeyConfigurationType.to_json f)
       ; Some ("MacAlgorithms", MacAlgorithmSpecList.to_json v.mac_algorithms)
       ; Aws.Util.option_map v.pending_deletion_window_in_days (fun f ->
             "PendingDeletionWindowInDays", Integer.to_json f)
       ; Aws.Util.option_map v.multi_region_configuration (fun f ->
             "MultiRegionConfiguration", MultiRegionConfiguration.to_json f)
       ; Aws.Util.option_map v.multi_region (fun f -> "MultiRegion", Boolean.to_json f)
       ; Some
           ( "KeyAgreementAlgorithms"
           , KeyAgreementAlgorithmSpecList.to_json v.key_agreement_algorithms )
       ; Some ("SigningAlgorithms", SigningAlgorithmSpecList.to_json v.signing_algorithms)
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
       ; Aws.Util.option_map v.deletion_date (fun f -> "DeletionDate", DateTime.to_json f)
       ; Aws.Util.option_map v.key_state (fun f -> "KeyState", KeyState.to_json f)
       ; Aws.Util.option_map v.key_usage (fun f -> "KeyUsage", KeyUsageType.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
       ; Aws.Util.option_map v.creation_date (fun f -> "CreationDate", DateTime.to_json f)
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
  ; key_agreement_algorithms =
      KeyAgreementAlgorithmSpecList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyAgreementAlgorithms"))
  ; multi_region = Aws.Util.option_map (Aws.Json.lookup j "MultiRegion") Boolean.of_json
  ; multi_region_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "MultiRegionConfiguration")
        MultiRegionConfiguration.of_json
  ; pending_deletion_window_in_days =
      Aws.Util.option_map
        (Aws.Json.lookup j "PendingDeletionWindowInDays")
        Integer.of_json
  ; mac_algorithms =
      MacAlgorithmSpecList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MacAlgorithms"))
  ; xks_key_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "XksKeyConfiguration")
        XksKeyConfigurationType.of_json
  ; current_key_material_id =
      Aws.Util.option_map (Aws.Json.lookup j "CurrentKeyMaterialId") String.of_json
  }
