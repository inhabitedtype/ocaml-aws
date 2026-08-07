open Aws.BaseTypes

type t =
  { destination_cidr_block : String.t option
  ; local_gateway_route_table_id : String.t
  ; local_gateway_virtual_interface_group_id : String.t option
  ; network_interface_id : String.t option
  ; dry_run : Boolean.t option
  ; destination_prefix_list_id : String.t option
  }

let make
    ?destination_cidr_block
    ~local_gateway_route_table_id
    ?local_gateway_virtual_interface_group_id
    ?network_interface_id
    ?dry_run
    ?destination_prefix_list_id
    () =
  { destination_cidr_block
  ; local_gateway_route_table_id
  ; local_gateway_virtual_interface_group_id
  ; network_interface_id
  ; dry_run
  ; destination_prefix_list_id
  }

let parse xml =
  Some
    { destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "DestinationCidrBlock" xml) String.parse
    ; local_gateway_route_table_id =
        Aws.Xml.required
          "LocalGatewayRouteTableId"
          (Aws.Util.option_bind
             (Aws.Xml.member "LocalGatewayRouteTableId" xml)
             String.parse)
    ; local_gateway_virtual_interface_group_id =
        Aws.Util.option_bind
          (Aws.Xml.member "LocalGatewayVirtualInterfaceGroupId" xml)
          String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "NetworkInterfaceId" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; destination_prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "DestinationPrefixListId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             Aws.Query.Pair ("DestinationPrefixListId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_id (fun f ->
             Aws.Query.Pair ("LocalGatewayVirtualInterfaceGroupId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("LocalGatewayRouteTableId", String.to_query v.local_gateway_route_table_id))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             "DestinationPrefixListId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "NetworkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_id (fun f ->
             "LocalGatewayVirtualInterfaceGroupId", String.to_json f)
       ; Some ("LocalGatewayRouteTableId", String.to_json v.local_gateway_route_table_id)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "DestinationCidrBlock", String.to_json f)
       ])

let of_json j =
  { destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationCidrBlock") String.of_json
  ; local_gateway_route_table_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LocalGatewayRouteTableId"))
  ; local_gateway_virtual_interface_group_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "LocalGatewayVirtualInterfaceGroupId")
        String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkInterfaceId") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; destination_prefix_list_id =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationPrefixListId") String.of_json
  }
