open Aws.BaseTypes

type t =
  { vpc_endpoint_connections : VpcEndpointConnectionSet.t
  ; next_token : String.t option
  }

let make ?(vpc_endpoint_connections = []) ?next_token () =
  { vpc_endpoint_connections; next_token }

let parse xml =
  Some
    { vpc_endpoint_connections =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "vpcEndpointConnectionSet" xml)
             VpcEndpointConnectionSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpcEndpointConnectionSet"
              , VpcEndpointConnectionSet.to_query v.vpc_endpoint_connections ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "vpcEndpointConnectionSet"
           , VpcEndpointConnectionSet.to_json v.vpc_endpoint_connections )
       ])

let of_json j =
  { vpc_endpoint_connections =
      VpcEndpointConnectionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcEndpointConnectionSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
