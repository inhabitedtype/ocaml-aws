open Aws.BaseTypes

type t =
  { custom_key_store_id : String.t
  ; new_custom_key_store_name : String.t option
  ; key_store_password : String.t option
  ; cloud_hsm_cluster_id : String.t option
  ; xks_proxy_uri_endpoint : String.t option
  ; xks_proxy_uri_path : String.t option
  ; xks_proxy_vpc_endpoint_service_name : String.t option
  ; xks_proxy_vpc_endpoint_service_owner : String.t option
  ; xks_proxy_authentication_credential : XksProxyAuthenticationCredentialType.t option
  ; xks_proxy_connectivity : XksProxyConnectivityType.t option
  }

let make
    ~custom_key_store_id
    ?new_custom_key_store_name
    ?key_store_password
    ?cloud_hsm_cluster_id
    ?xks_proxy_uri_endpoint
    ?xks_proxy_uri_path
    ?xks_proxy_vpc_endpoint_service_name
    ?xks_proxy_vpc_endpoint_service_owner
    ?xks_proxy_authentication_credential
    ?xks_proxy_connectivity
    () =
  { custom_key_store_id
  ; new_custom_key_store_name
  ; key_store_password
  ; cloud_hsm_cluster_id
  ; xks_proxy_uri_endpoint
  ; xks_proxy_uri_path
  ; xks_proxy_vpc_endpoint_service_name
  ; xks_proxy_vpc_endpoint_service_owner
  ; xks_proxy_authentication_credential
  ; xks_proxy_connectivity
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
    ; xks_proxy_uri_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "XksProxyUriEndpoint" xml) String.parse
    ; xks_proxy_uri_path =
        Aws.Util.option_bind (Aws.Xml.member "XksProxyUriPath" xml) String.parse
    ; xks_proxy_vpc_endpoint_service_name =
        Aws.Util.option_bind
          (Aws.Xml.member "XksProxyVpcEndpointServiceName" xml)
          String.parse
    ; xks_proxy_vpc_endpoint_service_owner =
        Aws.Util.option_bind
          (Aws.Xml.member "XksProxyVpcEndpointServiceOwner" xml)
          String.parse
    ; xks_proxy_authentication_credential =
        Aws.Util.option_bind
          (Aws.Xml.member "XksProxyAuthenticationCredential" xml)
          XksProxyAuthenticationCredentialType.parse
    ; xks_proxy_connectivity =
        Aws.Util.option_bind
          (Aws.Xml.member "XksProxyConnectivity" xml)
          XksProxyConnectivityType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.xks_proxy_connectivity (fun f ->
             Aws.Query.Pair ("XksProxyConnectivity", XksProxyConnectivityType.to_query f))
       ; Aws.Util.option_map v.xks_proxy_authentication_credential (fun f ->
             Aws.Query.Pair
               ( "XksProxyAuthenticationCredential"
               , XksProxyAuthenticationCredentialType.to_query f ))
       ; Aws.Util.option_map v.xks_proxy_vpc_endpoint_service_owner (fun f ->
             Aws.Query.Pair ("XksProxyVpcEndpointServiceOwner", String.to_query f))
       ; Aws.Util.option_map v.xks_proxy_vpc_endpoint_service_name (fun f ->
             Aws.Query.Pair ("XksProxyVpcEndpointServiceName", String.to_query f))
       ; Aws.Util.option_map v.xks_proxy_uri_path (fun f ->
             Aws.Query.Pair ("XksProxyUriPath", String.to_query f))
       ; Aws.Util.option_map v.xks_proxy_uri_endpoint (fun f ->
             Aws.Query.Pair ("XksProxyUriEndpoint", String.to_query f))
       ; Aws.Util.option_map v.cloud_hsm_cluster_id (fun f ->
             Aws.Query.Pair ("CloudHsmClusterId", String.to_query f))
       ; Aws.Util.option_map v.key_store_password (fun f ->
             Aws.Query.Pair ("KeyStorePassword", String.to_query f))
       ; Aws.Util.option_map v.new_custom_key_store_name (fun f ->
             Aws.Query.Pair ("NewCustomKeyStoreName", String.to_query f))
       ; Some (Aws.Query.Pair ("CustomKeyStoreId", String.to_query v.custom_key_store_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.xks_proxy_connectivity (fun f ->
             "XksProxyConnectivity", XksProxyConnectivityType.to_json f)
       ; Aws.Util.option_map v.xks_proxy_authentication_credential (fun f ->
             ( "XksProxyAuthenticationCredential"
             , XksProxyAuthenticationCredentialType.to_json f ))
       ; Aws.Util.option_map v.xks_proxy_vpc_endpoint_service_owner (fun f ->
             "XksProxyVpcEndpointServiceOwner", String.to_json f)
       ; Aws.Util.option_map v.xks_proxy_vpc_endpoint_service_name (fun f ->
             "XksProxyVpcEndpointServiceName", String.to_json f)
       ; Aws.Util.option_map v.xks_proxy_uri_path (fun f ->
             "XksProxyUriPath", String.to_json f)
       ; Aws.Util.option_map v.xks_proxy_uri_endpoint (fun f ->
             "XksProxyUriEndpoint", String.to_json f)
       ; Aws.Util.option_map v.cloud_hsm_cluster_id (fun f ->
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
  ; xks_proxy_uri_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "XksProxyUriEndpoint") String.of_json
  ; xks_proxy_uri_path =
      Aws.Util.option_map (Aws.Json.lookup j "XksProxyUriPath") String.of_json
  ; xks_proxy_vpc_endpoint_service_name =
      Aws.Util.option_map
        (Aws.Json.lookup j "XksProxyVpcEndpointServiceName")
        String.of_json
  ; xks_proxy_vpc_endpoint_service_owner =
      Aws.Util.option_map
        (Aws.Json.lookup j "XksProxyVpcEndpointServiceOwner")
        String.of_json
  ; xks_proxy_authentication_credential =
      Aws.Util.option_map
        (Aws.Json.lookup j "XksProxyAuthenticationCredential")
        XksProxyAuthenticationCredentialType.of_json
  ; xks_proxy_connectivity =
      Aws.Util.option_map
        (Aws.Json.lookup j "XksProxyConnectivity")
        XksProxyConnectivityType.of_json
  }
