open Aws.BaseTypes

type t =
  { instance_connect_endpoints : InstanceConnectEndpointSet.t
  ; next_token : String.t option
  }

let make ?(instance_connect_endpoints = []) ?next_token () =
  { instance_connect_endpoints; next_token }

let parse xml =
  Some
    { instance_connect_endpoints =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceConnectEndpointSet" xml)
             InstanceConnectEndpointSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceConnectEndpointSet"
              , InstanceConnectEndpointSet.to_query v.instance_connect_endpoints ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "instanceConnectEndpointSet"
           , InstanceConnectEndpointSet.to_json v.instance_connect_endpoints )
       ])

let of_json j =
  { instance_connect_endpoints =
      InstanceConnectEndpointSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceConnectEndpointSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
