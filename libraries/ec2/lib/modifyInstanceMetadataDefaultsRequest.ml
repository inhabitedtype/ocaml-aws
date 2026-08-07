open Aws.BaseTypes

type t =
  { http_tokens : MetadataDefaultHttpTokensState.t option
  ; http_put_response_hop_limit : Integer.t option
  ; http_endpoint : DefaultInstanceMetadataEndpointState.t option
  ; instance_metadata_tags : DefaultInstanceMetadataTagsState.t option
  ; dry_run : Boolean.t option
  ; http_tokens_enforced : DefaultHttpTokensEnforcedState.t option
  }

let make
    ?http_tokens
    ?http_put_response_hop_limit
    ?http_endpoint
    ?instance_metadata_tags
    ?dry_run
    ?http_tokens_enforced
    () =
  { http_tokens
  ; http_put_response_hop_limit
  ; http_endpoint
  ; instance_metadata_tags
  ; dry_run
  ; http_tokens_enforced
  }

let parse xml =
  Some
    { http_tokens =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpTokens" xml)
          MetadataDefaultHttpTokensState.parse
    ; http_put_response_hop_limit =
        Aws.Util.option_bind (Aws.Xml.member "HttpPutResponseHopLimit" xml) Integer.parse
    ; http_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpEndpoint" xml)
          DefaultInstanceMetadataEndpointState.parse
    ; instance_metadata_tags =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMetadataTags" xml)
          DefaultInstanceMetadataTagsState.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; http_tokens_enforced =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpTokensEnforced" xml)
          DefaultHttpTokensEnforcedState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.http_tokens_enforced (fun f ->
             Aws.Query.Pair
               ("HttpTokensEnforced", DefaultHttpTokensEnforcedState.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.instance_metadata_tags (fun f ->
             Aws.Query.Pair
               ("InstanceMetadataTags", DefaultInstanceMetadataTagsState.to_query f))
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             Aws.Query.Pair
               ("HttpEndpoint", DefaultInstanceMetadataEndpointState.to_query f))
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             Aws.Query.Pair ("HttpPutResponseHopLimit", Integer.to_query f))
       ; Aws.Util.option_map v.http_tokens (fun f ->
             Aws.Query.Pair ("HttpTokens", MetadataDefaultHttpTokensState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.http_tokens_enforced (fun f ->
             "HttpTokensEnforced", DefaultHttpTokensEnforcedState.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.instance_metadata_tags (fun f ->
             "InstanceMetadataTags", DefaultInstanceMetadataTagsState.to_json f)
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             "HttpEndpoint", DefaultInstanceMetadataEndpointState.to_json f)
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             "HttpPutResponseHopLimit", Integer.to_json f)
       ; Aws.Util.option_map v.http_tokens (fun f ->
             "HttpTokens", MetadataDefaultHttpTokensState.to_json f)
       ])

let of_json j =
  { http_tokens =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpTokens")
        MetadataDefaultHttpTokensState.of_json
  ; http_put_response_hop_limit =
      Aws.Util.option_map (Aws.Json.lookup j "HttpPutResponseHopLimit") Integer.of_json
  ; http_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpEndpoint")
        DefaultInstanceMetadataEndpointState.of_json
  ; instance_metadata_tags =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMetadataTags")
        DefaultInstanceMetadataTagsState.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; http_tokens_enforced =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpTokensEnforced")
        DefaultHttpTokensEnforcedState.of_json
  }
