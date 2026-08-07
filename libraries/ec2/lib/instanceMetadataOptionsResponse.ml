open Aws.BaseTypes

type t =
  { state : InstanceMetadataOptionsState.t option
  ; http_tokens : HttpTokensState.t option
  ; http_put_response_hop_limit : Integer.t option
  ; http_endpoint : InstanceMetadataEndpointState.t option
  ; http_protocol_ipv6 : InstanceMetadataProtocolState.t option
  ; instance_metadata_tags : InstanceMetadataTagsState.t option
  }

let make
    ?state
    ?http_tokens
    ?http_put_response_hop_limit
    ?http_endpoint
    ?http_protocol_ipv6
    ?instance_metadata_tags
    () =
  { state
  ; http_tokens
  ; http_put_response_hop_limit
  ; http_endpoint
  ; http_protocol_ipv6
  ; instance_metadata_tags
  }

let parse xml =
  Some
    { state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          InstanceMetadataOptionsState.parse
    ; http_tokens =
        Aws.Util.option_bind (Aws.Xml.member "httpTokens" xml) HttpTokensState.parse
    ; http_put_response_hop_limit =
        Aws.Util.option_bind (Aws.Xml.member "httpPutResponseHopLimit" xml) Integer.parse
    ; http_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "httpEndpoint" xml)
          InstanceMetadataEndpointState.parse
    ; http_protocol_ipv6 =
        Aws.Util.option_bind
          (Aws.Xml.member "httpProtocolIpv6" xml)
          InstanceMetadataProtocolState.parse
    ; instance_metadata_tags =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceMetadataTags" xml)
          InstanceMetadataTagsState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_metadata_tags (fun f ->
             Aws.Query.Pair ("InstanceMetadataTags", InstanceMetadataTagsState.to_query f))
       ; Aws.Util.option_map v.http_protocol_ipv6 (fun f ->
             Aws.Query.Pair ("HttpProtocolIpv6", InstanceMetadataProtocolState.to_query f))
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             Aws.Query.Pair ("HttpEndpoint", InstanceMetadataEndpointState.to_query f))
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             Aws.Query.Pair ("HttpPutResponseHopLimit", Integer.to_query f))
       ; Aws.Util.option_map v.http_tokens (fun f ->
             Aws.Query.Pair ("HttpTokens", HttpTokensState.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", InstanceMetadataOptionsState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_metadata_tags (fun f ->
             "instanceMetadataTags", InstanceMetadataTagsState.to_json f)
       ; Aws.Util.option_map v.http_protocol_ipv6 (fun f ->
             "httpProtocolIpv6", InstanceMetadataProtocolState.to_json f)
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             "httpEndpoint", InstanceMetadataEndpointState.to_json f)
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             "httpPutResponseHopLimit", Integer.to_json f)
       ; Aws.Util.option_map v.http_tokens (fun f ->
             "httpTokens", HttpTokensState.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", InstanceMetadataOptionsState.to_json f)
       ])

let of_json j =
  { state =
      Aws.Util.option_map (Aws.Json.lookup j "state") InstanceMetadataOptionsState.of_json
  ; http_tokens =
      Aws.Util.option_map (Aws.Json.lookup j "httpTokens") HttpTokensState.of_json
  ; http_put_response_hop_limit =
      Aws.Util.option_map (Aws.Json.lookup j "httpPutResponseHopLimit") Integer.of_json
  ; http_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "httpEndpoint")
        InstanceMetadataEndpointState.of_json
  ; http_protocol_ipv6 =
      Aws.Util.option_map
        (Aws.Json.lookup j "httpProtocolIpv6")
        InstanceMetadataProtocolState.of_json
  ; instance_metadata_tags =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceMetadataTags")
        InstanceMetadataTagsState.of_json
  }
