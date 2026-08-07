open Aws.BaseTypes

type t =
  { transit_gateway_multicast_domain_id : String.t option
  ; deregistered_network_interface_ids : ValueStringList.t
  ; group_ip_address : String.t option
  }

let make
    ?transit_gateway_multicast_domain_id
    ?(deregistered_network_interface_ids = [])
    ?group_ip_address
    () =
  { transit_gateway_multicast_domain_id
  ; deregistered_network_interface_ids
  ; group_ip_address
  }

let parse xml =
  Some
    { transit_gateway_multicast_domain_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayMulticastDomainId" xml)
          String.parse
    ; deregistered_network_interface_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "deregisteredNetworkInterfaceIds" xml)
             ValueStringList.parse)
    ; group_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "groupIpAddress" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_ip_address (fun f ->
             Aws.Query.Pair ("GroupIpAddress", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DeregisteredNetworkInterfaceIds"
              , ValueStringList.to_query v.deregistered_network_interface_ids ))
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             Aws.Query.Pair ("TransitGatewayMulticastDomainId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_ip_address (fun f ->
             "groupIpAddress", String.to_json f)
       ; Some
           ( "deregisteredNetworkInterfaceIds"
           , ValueStringList.to_json v.deregistered_network_interface_ids )
       ; Aws.Util.option_map v.transit_gateway_multicast_domain_id (fun f ->
             "transitGatewayMulticastDomainId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_multicast_domain_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayMulticastDomainId")
        String.of_json
  ; deregistered_network_interface_ids =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "deregisteredNetworkInterfaceIds"))
  ; group_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "groupIpAddress") String.of_json
  }
