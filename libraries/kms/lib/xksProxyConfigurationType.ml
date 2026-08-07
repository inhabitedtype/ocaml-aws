open Aws.BaseTypes

type t =
  { connectivity : XksProxyConnectivityType.t option
  ; access_key_id : String.t option
  ; uri_endpoint : String.t option
  ; uri_path : String.t option
  ; vpc_endpoint_service_name : String.t option
  ; vpc_endpoint_service_owner : String.t option
  }

let make
    ?connectivity
    ?access_key_id
    ?uri_endpoint
    ?uri_path
    ?vpc_endpoint_service_name
    ?vpc_endpoint_service_owner
    () =
  { connectivity
  ; access_key_id
  ; uri_endpoint
  ; uri_path
  ; vpc_endpoint_service_name
  ; vpc_endpoint_service_owner
  }

let parse xml =
  Some
    { connectivity =
        Aws.Util.option_bind
          (Aws.Xml.member "Connectivity" xml)
          XksProxyConnectivityType.parse
    ; access_key_id = Aws.Util.option_bind (Aws.Xml.member "AccessKeyId" xml) String.parse
    ; uri_endpoint = Aws.Util.option_bind (Aws.Xml.member "UriEndpoint" xml) String.parse
    ; uri_path = Aws.Util.option_bind (Aws.Xml.member "UriPath" xml) String.parse
    ; vpc_endpoint_service_name =
        Aws.Util.option_bind (Aws.Xml.member "VpcEndpointServiceName" xml) String.parse
    ; vpc_endpoint_service_owner =
        Aws.Util.option_bind (Aws.Xml.member "VpcEndpointServiceOwner" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_endpoint_service_owner (fun f ->
             Aws.Query.Pair ("VpcEndpointServiceOwner", String.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_service_name (fun f ->
             Aws.Query.Pair ("VpcEndpointServiceName", String.to_query f))
       ; Aws.Util.option_map v.uri_path (fun f ->
             Aws.Query.Pair ("UriPath", String.to_query f))
       ; Aws.Util.option_map v.uri_endpoint (fun f ->
             Aws.Query.Pair ("UriEndpoint", String.to_query f))
       ; Aws.Util.option_map v.access_key_id (fun f ->
             Aws.Query.Pair ("AccessKeyId", String.to_query f))
       ; Aws.Util.option_map v.connectivity (fun f ->
             Aws.Query.Pair ("Connectivity", XksProxyConnectivityType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_endpoint_service_owner (fun f ->
             "VpcEndpointServiceOwner", String.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_service_name (fun f ->
             "VpcEndpointServiceName", String.to_json f)
       ; Aws.Util.option_map v.uri_path (fun f -> "UriPath", String.to_json f)
       ; Aws.Util.option_map v.uri_endpoint (fun f -> "UriEndpoint", String.to_json f)
       ; Aws.Util.option_map v.access_key_id (fun f -> "AccessKeyId", String.to_json f)
       ; Aws.Util.option_map v.connectivity (fun f ->
             "Connectivity", XksProxyConnectivityType.to_json f)
       ])

let of_json j =
  { connectivity =
      Aws.Util.option_map
        (Aws.Json.lookup j "Connectivity")
        XksProxyConnectivityType.of_json
  ; access_key_id = Aws.Util.option_map (Aws.Json.lookup j "AccessKeyId") String.of_json
  ; uri_endpoint = Aws.Util.option_map (Aws.Json.lookup j "UriEndpoint") String.of_json
  ; uri_path = Aws.Util.option_map (Aws.Json.lookup j "UriPath") String.of_json
  ; vpc_endpoint_service_name =
      Aws.Util.option_map (Aws.Json.lookup j "VpcEndpointServiceName") String.of_json
  ; vpc_endpoint_service_owner =
      Aws.Util.option_map (Aws.Json.lookup j "VpcEndpointServiceOwner") String.of_json
  }
