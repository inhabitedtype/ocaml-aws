open Aws.BaseTypes

type t =
  { transit_gateway_multicast_domain_id : String.t
  ; group_ip_address : String.t option
  ; network_interface_ids : TransitGatewayNetworkInterfaceIdList.t
  ; dry_run : Boolean.t option
  }

let make
    ~transit_gateway_multicast_domain_id
    ?group_ip_address
    ~network_interface_ids
    ?dry_run
    () =
  { transit_gateway_multicast_domain_id
  ; group_ip_address
  ; network_interface_ids
  ; dry_run
  }

let parse xml =
  Some
    { transit_gateway_multicast_domain_id =
        Aws.Xml.required
          "TransitGatewayMulticastDomainId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayMulticastDomainId" xml)
             String.parse)
    ; group_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "GroupIpAddress" xml) String.parse
    ; network_interface_ids =
        Aws.Xml.required
          "NetworkInterfaceIds"
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInterfaceIds" xml)
             TransitGatewayNetworkInterfaceIdList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterfaceIds"
              , TransitGatewayNetworkInterfaceIdList.to_query v.network_interface_ids ))
       ; Aws.Util.option_map v.group_ip_address (fun f ->
             Aws.Query.Pair ("GroupIpAddress", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayMulticastDomainId"
              , String.to_query v.transit_gateway_multicast_domain_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "NetworkInterfaceIds"
           , TransitGatewayNetworkInterfaceIdList.to_json v.network_interface_ids )
       ; Aws.Util.option_map v.group_ip_address (fun f ->
             "GroupIpAddress", String.to_json f)
       ; Some
           ( "TransitGatewayMulticastDomainId"
           , String.to_json v.transit_gateway_multicast_domain_id )
       ])

let of_json j =
  { transit_gateway_multicast_domain_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayMulticastDomainId"))
  ; group_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "GroupIpAddress") String.of_json
  ; network_interface_ids =
      TransitGatewayNetworkInterfaceIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInterfaceIds"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
