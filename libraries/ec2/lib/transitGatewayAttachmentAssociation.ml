open Aws.BaseTypes

type t =
  { transit_gateway_route_table_id : String.t option
  ; state : TransitGatewayAssociationState.t option
  }

let make ?transit_gateway_route_table_id ?state () =
  { transit_gateway_route_table_id; state }

let parse xml =
  Some
    { transit_gateway_route_table_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableId" xml)
          String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayAssociationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayAssociationState.to_query f))
       ; Aws.Util.option_map v.transit_gateway_route_table_id (fun f ->
             Aws.Query.Pair ("TransitGatewayRouteTableId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayAssociationState.to_json f)
       ; Aws.Util.option_map v.transit_gateway_route_table_id (fun f ->
             "transitGatewayRouteTableId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayRouteTableId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayAssociationState.of_json
  }
