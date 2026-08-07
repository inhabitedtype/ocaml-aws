open Aws.BaseTypes

type t =
  { vpc_endpoints : VpcEndpointSet.t
  ; next_token : String.t option
  }

let make ?(vpc_endpoints = []) ?next_token () = { vpc_endpoints; next_token }

let parse xml =
  Some
    { vpc_endpoints =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "vpcEndpointSet" xml)
             VpcEndpointSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("VpcEndpointSet", VpcEndpointSet.to_query v.vpc_endpoints))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("vpcEndpointSet", VpcEndpointSet.to_json v.vpc_endpoints)
       ])

let of_json j =
  { vpc_endpoints =
      VpcEndpointSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcEndpointSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
