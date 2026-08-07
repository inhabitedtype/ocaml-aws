open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; http_tokens : HttpTokensState.t option
  ; http_put_response_hop_limit : Integer.t option
  ; http_endpoint : InstanceMetadataEndpointState.t option
  ; dry_run : Boolean.t option
  ; http_protocol_ipv6 : InstanceMetadataProtocolState.t option
  ; instance_metadata_tags : InstanceMetadataTagsState.t option
  }

let make
    ~instance_id
    ?http_tokens
    ?http_put_response_hop_limit
    ?http_endpoint
    ?dry_run
    ?http_protocol_ipv6
    ?instance_metadata_tags
    () =
  { instance_id
  ; http_tokens
  ; http_put_response_hop_limit
  ; http_endpoint
  ; dry_run
  ; http_protocol_ipv6
  ; instance_metadata_tags
  }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; http_tokens =
        Aws.Util.option_bind (Aws.Xml.member "HttpTokens" xml) HttpTokensState.parse
    ; http_put_response_hop_limit =
        Aws.Util.option_bind (Aws.Xml.member "HttpPutResponseHopLimit" xml) Integer.parse
    ; http_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpEndpoint" xml)
          InstanceMetadataEndpointState.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; http_protocol_ipv6 =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpProtocolIpv6" xml)
          InstanceMetadataProtocolState.parse
    ; instance_metadata_tags =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMetadataTags" xml)
          InstanceMetadataTagsState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_metadata_tags (fun f ->
             Aws.Query.Pair ("InstanceMetadataTags", InstanceMetadataTagsState.to_query f))
       ; Aws.Util.option_map v.http_protocol_ipv6 (fun f ->
             Aws.Query.Pair ("HttpProtocolIpv6", InstanceMetadataProtocolState.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             Aws.Query.Pair ("HttpEndpoint", InstanceMetadataEndpointState.to_query f))
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             Aws.Query.Pair ("HttpPutResponseHopLimit", Integer.to_query f))
       ; Aws.Util.option_map v.http_tokens (fun f ->
             Aws.Query.Pair ("HttpTokens", HttpTokensState.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_metadata_tags (fun f ->
             "InstanceMetadataTags", InstanceMetadataTagsState.to_json f)
       ; Aws.Util.option_map v.http_protocol_ipv6 (fun f ->
             "HttpProtocolIpv6", InstanceMetadataProtocolState.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             "HttpEndpoint", InstanceMetadataEndpointState.to_json f)
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             "HttpPutResponseHopLimit", Integer.to_json f)
       ; Aws.Util.option_map v.http_tokens (fun f ->
             "HttpTokens", HttpTokensState.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; http_tokens =
      Aws.Util.option_map (Aws.Json.lookup j "HttpTokens") HttpTokensState.of_json
  ; http_put_response_hop_limit =
      Aws.Util.option_map (Aws.Json.lookup j "HttpPutResponseHopLimit") Integer.of_json
  ; http_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpEndpoint")
        InstanceMetadataEndpointState.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; http_protocol_ipv6 =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpProtocolIpv6")
        InstanceMetadataProtocolState.of_json
  ; instance_metadata_tags =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMetadataTags")
        InstanceMetadataTagsState.of_json
  }
