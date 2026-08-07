open Aws.BaseTypes

type t =
  { local_gateway_route_table_virtual_interface_group_association_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~local_gateway_route_table_virtual_interface_group_association_id ?dry_run () =
  { local_gateway_route_table_virtual_interface_group_association_id; dry_run }

let parse xml =
  Some
    { local_gateway_route_table_virtual_interface_group_association_id =
        Aws.Xml.required
          "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"
          (Aws.Util.option_bind
             (Aws.Xml.member
                "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"
                xml)
             String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"
              , String.to_query
                  v.local_gateway_route_table_virtual_interface_group_association_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"
           , String.to_json
               v.local_gateway_route_table_virtual_interface_group_association_id )
       ])

let of_json j =
  { local_gateway_route_table_virtual_interface_group_association_id =
      String.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
