open Aws.BaseTypes

type t =
  { http_tokens : InstanceMetadataHttpTokensState.t option
  ; http_put_response_hop_limit : Integer.t option
  ; http_endpoint : InstanceMetadataEndpointState.t option
  }

let make ?http_tokens ?http_put_response_hop_limit ?http_endpoint () =
  { http_tokens; http_put_response_hop_limit; http_endpoint }

let parse xml =
  Some
    { http_tokens =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpTokens" xml)
          InstanceMetadataHttpTokensState.parse
    ; http_put_response_hop_limit =
        Aws.Util.option_bind (Aws.Xml.member "HttpPutResponseHopLimit" xml) Integer.parse
    ; http_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "HttpEndpoint" xml)
          InstanceMetadataEndpointState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.http_endpoint (fun f ->
             Aws.Query.Pair ("HttpEndpoint", InstanceMetadataEndpointState.to_query f))
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             Aws.Query.Pair ("HttpPutResponseHopLimit", Integer.to_query f))
       ; Aws.Util.option_map v.http_tokens (fun f ->
             Aws.Query.Pair ("HttpTokens", InstanceMetadataHttpTokensState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.http_endpoint (fun f ->
             "HttpEndpoint", InstanceMetadataEndpointState.to_json f)
       ; Aws.Util.option_map v.http_put_response_hop_limit (fun f ->
             "HttpPutResponseHopLimit", Integer.to_json f)
       ; Aws.Util.option_map v.http_tokens (fun f ->
             "HttpTokens", InstanceMetadataHttpTokensState.to_json f)
       ])

let of_json j =
  { http_tokens =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpTokens")
        InstanceMetadataHttpTokensState.of_json
  ; http_put_response_hop_limit =
      Aws.Util.option_map (Aws.Json.lookup j "HttpPutResponseHopLimit") Integer.of_json
  ; http_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "HttpEndpoint")
        InstanceMetadataEndpointState.of_json
  }
