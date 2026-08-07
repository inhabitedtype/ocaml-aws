open Aws.BaseTypes

type t =
  { client_vpn_endpoints : EndpointSet.t
  ; next_token : String.t option
  }

let make ?(client_vpn_endpoints = []) ?next_token () =
  { client_vpn_endpoints; next_token }

let parse xml =
  Some
    { client_vpn_endpoints =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "clientVpnEndpoint" xml)
             EndpointSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpoint", EndpointSet.to_query v.client_vpn_endpoints))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("clientVpnEndpoint", EndpointSet.to_json v.client_vpn_endpoints)
       ])

let of_json j =
  { client_vpn_endpoints =
      EndpointSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "clientVpnEndpoint"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
