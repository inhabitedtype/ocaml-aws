type t = { local_gateway_virtual_interface : LocalGatewayVirtualInterface.t option }

let make ?local_gateway_virtual_interface () = { local_gateway_virtual_interface }

let parse xml =
  Some
    { local_gateway_virtual_interface =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayVirtualInterface" xml)
          LocalGatewayVirtualInterface.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_virtual_interface (fun f ->
             Aws.Query.Pair
               ("LocalGatewayVirtualInterface", LocalGatewayVirtualInterface.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_virtual_interface (fun f ->
             "localGatewayVirtualInterface", LocalGatewayVirtualInterface.to_json f)
       ])

let of_json j =
  { local_gateway_virtual_interface =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayVirtualInterface")
        LocalGatewayVirtualInterface.of_json
  }
