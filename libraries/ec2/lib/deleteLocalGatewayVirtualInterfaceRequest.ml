open Aws.BaseTypes

type t =
  { local_gateway_virtual_interface_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~local_gateway_virtual_interface_id ?dry_run () =
  { local_gateway_virtual_interface_id; dry_run }

let parse xml =
  Some
    { local_gateway_virtual_interface_id =
        Aws.Xml.required
          "LocalGatewayVirtualInterfaceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "LocalGatewayVirtualInterfaceId" xml)
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
              ( "LocalGatewayVirtualInterfaceId"
              , String.to_query v.local_gateway_virtual_interface_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "LocalGatewayVirtualInterfaceId"
           , String.to_json v.local_gateway_virtual_interface_id )
       ])

let of_json j =
  { local_gateway_virtual_interface_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LocalGatewayVirtualInterfaceId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
