open Aws.BaseTypes

type t =
  { transit_gateway_route_table_id : String.t
  ; destination_cidr_block : String.t
  ; dry_run : Boolean.t option
  }

let make ~transit_gateway_route_table_id ~destination_cidr_block ?dry_run () =
  { transit_gateway_route_table_id; destination_cidr_block; dry_run }

let parse xml =
  Some
    { transit_gateway_route_table_id =
        Aws.Xml.required
          "TransitGatewayRouteTableId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayRouteTableId" xml)
             String.parse)
    ; destination_cidr_block =
        Aws.Xml.required
          "DestinationCidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "DestinationCidrBlock" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DestinationCidrBlock", String.to_query v.destination_cidr_block))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayRouteTableId"
              , String.to_query v.transit_gateway_route_table_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("DestinationCidrBlock", String.to_json v.destination_cidr_block)
       ; Some
           ("TransitGatewayRouteTableId", String.to_json v.transit_gateway_route_table_id)
       ])

let of_json j =
  { transit_gateway_route_table_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayRouteTableId"))
  ; destination_cidr_block =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationCidrBlock"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
