open Aws.BaseTypes

type t =
  { http_tokens : HttpTokensState.t option
  ; http_put_response_hop_limit : Integer.t option
  ; http_endpoint : InstanceMetadataEndpointState.t option
  ; instance_metadata_tags : InstanceMetadataTagsState.t option
  ; managed_by : ManagedBy.t option
  ; managed_exception_message : String.t option
  ; http_tokens_enforced : HttpTokensEnforcedState.t option
  }

let make
    ?http_tokens
    ?http_put_response_hop_limit
    ?http_endpoint
    ?instance_metadata_tags
    ?managed_by
    ?managed_exception_message
    ?http_tokens_enforced
    () =
  { http_tokens
  ; http_put_response_hop_limit
  ; http_endpoint
  ; instance_metadata_tags
  ; managed_by
  ; managed_exception_message
  ; http_tokens_enforced
  }

let parse xml =
  Some
    { http_tokens =
        Aws.Util.option_bind (Aws.Xml.member "httpTokens" xml) HttpTokensState.parse
    ; http_put_response_hop_limit =
        Aws.Util.option_bind (Aws.Xml.member "httpPutResponseHopLimit" xml) Integer.parse
    ; http_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "httpEndpoint" xml)
          InstanceMetadataEndpointState.parse
    ; instance_metadata_tags =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceMetadataTags" xml)
          InstanceMetadataTagsState.parse
    ; managed_by = Aws.Util.option_bind (Aws.Xml.member "managedBy" xml) ManagedBy.parse
    ; managed_exception_message =
        Aws.Util.option_bind (Aws.Xml.member "managedExceptionMessage" xml) String.parse
    ; http_tokens_enforced =
        Aws.Util.option_bind
          (Aws.Xml.member "httpTokensEnforced" xml)
          HttpTokensEnforcedState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.http_tokens_enforced (fun f ->
             Aws.Query.Pair ("HttpTokensEnforced", HttpTokensEnforcedState.to_query f))
       ; Aws.Util.option_map v.managed_exception_message (fun f ->
             Aws.Query.Pair ("ManagedExceptionMessage", String.to_query f))
       ; Aws.Util.option_map v.managed_by (fun f ->
             Aws.Query.Pair ("ManagedBy", ManagedBy.to_query f))
       ; Aws.Util.option_map v.instance_metadata_tags (fun f ->
             Aws.Query.Pair ("InstanceMetadataTags", InstanceMetadataTagsState.to_query f))
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             Aws.Query.Pair ("HttpEndpoint", InstanceMetadataEndpointState.to_query f))
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             Aws.Query.Pair ("HttpPutResponseHopLimit", Integer.to_query f))
       ; Aws.Util.option_map v.http_tokens (fun f ->
             Aws.Query.Pair ("HttpTokens", HttpTokensState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.http_tokens_enforced (fun f ->
             "httpTokensEnforced", HttpTokensEnforcedState.to_json f)
       ; Aws.Util.option_map v.managed_exception_message (fun f ->
             "managedExceptionMessage", String.to_json f)
       ; Aws.Util.option_map v.managed_by (fun f -> "managedBy", ManagedBy.to_json f)
       ; Aws.Util.option_map v.instance_metadata_tags (fun f ->
             "instanceMetadataTags", InstanceMetadataTagsState.to_json f)
       ; Aws.Util.option_map v.http_endpoint (fun f ->
             "httpEndpoint", InstanceMetadataEndpointState.to_json f)
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             "httpPutResponseHopLimit", Integer.to_json f)
       ; Aws.Util.option_map v.http_tokens (fun f ->
             "httpTokens", HttpTokensState.to_json f)
       ])

let of_json j =
  { http_tokens =
      Aws.Util.option_map (Aws.Json.lookup j "httpTokens") HttpTokensState.of_json
  ; http_put_response_hop_limit =
      Aws.Util.option_map (Aws.Json.lookup j "httpPutResponseHopLimit") Integer.of_json
  ; http_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "httpEndpoint")
        InstanceMetadataEndpointState.of_json
  ; instance_metadata_tags =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceMetadataTags")
        InstanceMetadataTagsState.of_json
  ; managed_by = Aws.Util.option_map (Aws.Json.lookup j "managedBy") ManagedBy.of_json
  ; managed_exception_message =
      Aws.Util.option_map (Aws.Json.lookup j "managedExceptionMessage") String.of_json
  ; http_tokens_enforced =
      Aws.Util.option_map
        (Aws.Json.lookup j "httpTokensEnforced")
        HttpTokensEnforcedState.of_json
  }
