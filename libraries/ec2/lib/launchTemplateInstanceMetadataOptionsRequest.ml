open Aws.BaseTypes

type t =
  { http_tokens : LaunchTemplateHttpTokensState.t option
  ; http_put_response_hop_limit : Integer.t option
  ; http_endpoint : LaunchTemplateInstanceMetadataEndpointState.t option
  ; http_protocol_ipv6 : LaunchTemplateInstanceMetadataProtocolIpv6.t option
  ; instance_metadata_tags : LaunchTemplateInstanceMetadataTagsState.t option
  }

let make
    ?http_tokens
    ?http_put_response_hop_limit
    ?http_endpoint
    ?http_protocol_ipv6
    ?instance_metadata_tags
    () =
  { http_tokens
  ; http_put_response_hop_limit
  ; http_endpoint
  ; http_protocol_ipv6
  ; instance_metadata_tags
  }

let parse xml =
  Some
    { http_tokens =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpTokens" xml)
          LaunchTemplateHttpTokensState.parse
    ; http_put_response_hop_limit =
        Aws.Util.option_bind (Aws.Xml.member "HttpPutResponseHopLimit" xml) Integer.parse
    ; http_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpEndpoint" xml)
          LaunchTemplateInstanceMetadataEndpointState.parse
    ; http_protocol_ipv6 =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpProtocolIpv6" xml)
          LaunchTemplateInstanceMetadataProtocolIpv6.parse
    ; instance_metadata_tags =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMetadataTags" xml)
          LaunchTemplateInstanceMetadataTagsState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_metadata_tags (fun f ->
             Aws.Query.Pair
               ("InstanceMetadataTags", LaunchTemplateInstanceMetadataTagsState.to_query f))
       ; Aws.Util.option_map v.http_protocol_ipv6 (fun f ->
             Aws.Query.Pair
               ("HttpProtocolIpv6", LaunchTemplateInstanceMetadataProtocolIpv6.to_query f))
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             Aws.Query.Pair
               ("HttpEndpoint", LaunchTemplateInstanceMetadataEndpointState.to_query f))
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             Aws.Query.Pair ("HttpPutResponseHopLimit", Integer.to_query f))
       ; Aws.Util.option_map v.http_tokens (fun f ->
             Aws.Query.Pair ("HttpTokens", LaunchTemplateHttpTokensState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_metadata_tags (fun f ->
             "InstanceMetadataTags", LaunchTemplateInstanceMetadataTagsState.to_json f)
       ; Aws.Util.option_map v.http_protocol_ipv6 (fun f ->
             "HttpProtocolIpv6", LaunchTemplateInstanceMetadataProtocolIpv6.to_json f)
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             "HttpEndpoint", LaunchTemplateInstanceMetadataEndpointState.to_json f)
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             "HttpPutResponseHopLimit", Integer.to_json f)
       ; Aws.Util.option_map v.http_tokens (fun f ->
             "HttpTokens", LaunchTemplateHttpTokensState.to_json f)
       ])

let of_json j =
  { http_tokens =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpTokens")
        LaunchTemplateHttpTokensState.of_json
  ; http_put_response_hop_limit =
      Aws.Util.option_map (Aws.Json.lookup j "HttpPutResponseHopLimit") Integer.of_json
  ; http_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpEndpoint")
        LaunchTemplateInstanceMetadataEndpointState.of_json
  ; http_protocol_ipv6 =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpProtocolIpv6")
        LaunchTemplateInstanceMetadataProtocolIpv6.of_json
  ; instance_metadata_tags =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMetadataTags")
        LaunchTemplateInstanceMetadataTagsState.of_json
  }
