type t =
  { local_gateway_virtual_interface_group : LocalGatewayVirtualInterfaceGroup.t option }

let make ?local_gateway_virtual_interface_group () =
  { local_gateway_virtual_interface_group }

let parse xml =
  Some
    { local_gateway_virtual_interface_group =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayVirtualInterfaceGroup" xml)
          LocalGatewayVirtualInterfaceGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_virtual_interface_group (fun f ->
             Aws.Query.Pair
               ( "LocalGatewayVirtualInterfaceGroup"
               , LocalGatewayVirtualInterfaceGroup.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_virtual_interface_group (fun f ->
             ( "localGatewayVirtualInterfaceGroup"
             , LocalGatewayVirtualInterfaceGroup.to_json f ))
       ])

let of_json j =
  { local_gateway_virtual_interface_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayVirtualInterfaceGroup")
        LocalGatewayVirtualInterfaceGroup.of_json
  }
