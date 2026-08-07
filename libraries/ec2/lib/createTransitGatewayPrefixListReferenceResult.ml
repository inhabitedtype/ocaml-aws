type t =
  { transit_gateway_prefix_list_reference : TransitGatewayPrefixListReference.t option }

let make ?transit_gateway_prefix_list_reference () =
  { transit_gateway_prefix_list_reference }

let parse xml =
  Some
    { transit_gateway_prefix_list_reference =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayPrefixListReference" xml)
          TransitGatewayPrefixListReference.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_prefix_list_reference (fun f ->
             Aws.Query.Pair
               ( "TransitGatewayPrefixListReference"
               , TransitGatewayPrefixListReference.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_prefix_list_reference (fun f ->
             ( "transitGatewayPrefixListReference"
             , TransitGatewayPrefixListReference.to_json f ))
       ])

let of_json j =
  { transit_gateway_prefix_list_reference =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayPrefixListReference")
        TransitGatewayPrefixListReference.of_json
  }
