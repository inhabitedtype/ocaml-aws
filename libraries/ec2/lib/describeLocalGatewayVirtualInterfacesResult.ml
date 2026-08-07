open Aws.BaseTypes

type t =
  { local_gateway_virtual_interfaces : LocalGatewayVirtualInterfaceSet.t
  ; next_token : String.t option
  }

let make ?(local_gateway_virtual_interfaces = []) ?next_token () =
  { local_gateway_virtual_interfaces; next_token }

let parse xml =
  Some
    { local_gateway_virtual_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "localGatewayVirtualInterfaceSet" xml)
             LocalGatewayVirtualInterfaceSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LocalGatewayVirtualInterfaceSet"
              , LocalGatewayVirtualInterfaceSet.to_query
                  v.local_gateway_virtual_interfaces ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "localGatewayVirtualInterfaceSet"
           , LocalGatewayVirtualInterfaceSet.to_json v.local_gateway_virtual_interfaces )
       ])

let of_json j =
  { local_gateway_virtual_interfaces =
      LocalGatewayVirtualInterfaceSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "localGatewayVirtualInterfaceSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
