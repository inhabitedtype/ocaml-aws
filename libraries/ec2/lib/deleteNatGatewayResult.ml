open Aws.BaseTypes

type t = { nat_gateway_id : String.t option }

let make ?nat_gateway_id () = { nat_gateway_id }

let parse xml =
  Some
    { nat_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "natGatewayId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nat_gateway_id (fun f ->
             Aws.Query.Pair ("NatGatewayId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nat_gateway_id (fun f -> "natGatewayId", String.to_json f)
       ])

let of_json j =
  { nat_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "natGatewayId") String.of_json
  }
