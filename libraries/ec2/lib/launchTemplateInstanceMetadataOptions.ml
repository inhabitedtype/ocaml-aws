open Aws.BaseTypes

type t =
  { state : LaunchTemplateInstanceMetadataOptionsState.t option
  ; http_tokens : LaunchTemplateHttpTokensState.t option
  ; http_put_response_hop_limit : Integer.t option
  ; http_endpoint : LaunchTemplateInstanceMetadataEndpointState.t option
  ; http_protocol_ipv6 : LaunchTemplateInstanceMetadataProtocolIpv6.t option
  ; instance_metadata_tags : LaunchTemplateInstanceMetadataTagsState.t option
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
          LaunchTemplateInstanceMetadataOptionsState.parse
    ; http_tokens =
        Aws.Util.option_bind
          (Aws.Xml.member "httpTokens" xml)
          LaunchTemplateHttpTokensState.parse
    ; http_put_response_hop_limit =
        Aws.Util.option_bind (Aws.Xml.member "httpPutResponseHopLimit" xml) Integer.parse
    ; http_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "httpEndpoint" xml)
          LaunchTemplateInstanceMetadataEndpointState.parse
    ; http_protocol_ipv6 =
        Aws.Util.option_bind
          (Aws.Xml.member "httpProtocolIpv6" xml)
          LaunchTemplateInstanceMetadataProtocolIpv6.parse
    ; instance_metadata_tags =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceMetadataTags" xml)
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
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair
               ("State", LaunchTemplateInstanceMetadataOptionsState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_metadata_tags (fun f ->
             "instanceMetadataTags", LaunchTemplateInstanceMetadataTagsState.to_json f)
       ; Aws.Util.option_map v.http_protocol_ipv6 (fun f ->
             "httpProtocolIpv6", LaunchTemplateInstanceMetadataProtocolIpv6.to_json f)
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             "httpEndpoint", LaunchTemplateInstanceMetadataEndpointState.to_json f)
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             "httpPutResponseHopLimit", Integer.to_json f)
       ; Aws.Util.option_map v.http_tokens (fun f ->
             "httpTokens", LaunchTemplateHttpTokensState.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", LaunchTemplateInstanceMetadataOptionsState.to_json f)
       ])

let of_json j =
  { state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        LaunchTemplateInstanceMetadataOptionsState.of_json
  ; http_tokens =
      Aws.Util.option_map
        (Aws.Json.lookup j "httpTokens")
        LaunchTemplateHttpTokensState.of_json
  ; http_put_response_hop_limit =
      Aws.Util.option_map (Aws.Json.lookup j "httpPutResponseHopLimit") Integer.of_json
  ; http_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "httpEndpoint")
        LaunchTemplateInstanceMetadataEndpointState.of_json
  ; http_protocol_ipv6 =
      Aws.Util.option_map
        (Aws.Json.lookup j "httpProtocolIpv6")
        LaunchTemplateInstanceMetadataProtocolIpv6.of_json
  ; instance_metadata_tags =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceMetadataTags")
        LaunchTemplateInstanceMetadataTagsState.of_json
  }
