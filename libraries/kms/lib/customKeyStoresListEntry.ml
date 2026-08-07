open Aws.BaseTypes

type t =
  { custom_key_store_id : String.t option
  ; custom_key_store_name : String.t option
  ; cloud_hsm_cluster_id : String.t option
  ; trust_anchor_certificate : String.t option
  ; connection_state : ConnectionStateType.t option
  ; connection_error_code : ConnectionErrorCodeType.t option
  ; creation_date : DateTime.t option
  ; custom_key_store_type : CustomKeyStoreType.t option
  ; xks_proxy_configuration : XksProxyConfigurationType.t option
  }

let make
    ?custom_key_store_id
    ?custom_key_store_name
    ?cloud_hsm_cluster_id
    ?trust_anchor_certificate
    ?connection_state
    ?connection_error_code
    ?creation_date
    ?custom_key_store_type
    ?xks_proxy_configuration
    () =
  { custom_key_store_id
  ; custom_key_store_name
  ; cloud_hsm_cluster_id
  ; trust_anchor_certificate
  ; connection_state
  ; connection_error_code
  ; creation_date
  ; custom_key_store_type
  ; xks_proxy_configuration
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
    ; custom_key_store_type =
        Aws.Util.option_bind
          (Aws.Xml.member "CustomKeyStoreType" xml)
          CustomKeyStoreType.parse
    ; xks_proxy_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "XksProxyConfiguration" xml)
          XksProxyConfigurationType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.xks_proxy_configuration (fun f ->
             Aws.Query.Pair ("XksProxyConfiguration", XksProxyConfigurationType.to_query f))
       ; Aws.Util.option_map v.custom_key_store_type (fun f ->
             Aws.Query.Pair ("CustomKeyStoreType", CustomKeyStoreType.to_query f))
       ; Aws.Util.option_map v.creation_date (fun f ->
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
       [ Aws.Util.option_map v.xks_proxy_configuration (fun f ->
             "XksProxyConfiguration", XksProxyConfigurationType.to_json f)
       ; Aws.Util.option_map v.custom_key_store_type (fun f ->
             "CustomKeyStoreType", CustomKeyStoreType.to_json f)
       ; Aws.Util.option_map v.creation_date (fun f -> "CreationDate", DateTime.to_json f)
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
  ; custom_key_store_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "CustomKeyStoreType")
        CustomKeyStoreType.of_json
  ; xks_proxy_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "XksProxyConfiguration")
        XksProxyConfigurationType.of_json
  }
