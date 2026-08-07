open Aws.BaseTypes

type t =
  { resource_arn : String.t option
  ; http_endpoint_enabled : Boolean.t option
  }

let make ?resource_arn ?http_endpoint_enabled () = { resource_arn; http_endpoint_enabled }

let parse xml =
  Some
    { resource_arn = Aws.Util.option_bind (Aws.Xml.member "ResourceArn" xml) String.parse
    ; http_endpoint_enabled =
        Aws.Util.option_bind (Aws.Xml.member "HttpEndpointEnabled" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.http_endpoint_enabled (fun f ->
             Aws.Query.Pair ("HttpEndpointEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.resource_arn (fun f ->
             Aws.Query.Pair ("ResourceArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.http_endpoint_enabled (fun f ->
             "HttpEndpointEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.resource_arn (fun f -> "ResourceArn", String.to_json f)
       ])

let of_json j =
  { resource_arn = Aws.Util.option_map (Aws.Json.lookup j "ResourceArn") String.of_json
  ; http_endpoint_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "HttpEndpointEnabled") Boolean.of_json
  }
