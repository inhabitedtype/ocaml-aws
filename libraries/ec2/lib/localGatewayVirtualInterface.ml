open Aws.BaseTypes

type t =
  { local_gateway_virtual_interface_id : String.t option
  ; local_gateway_id : String.t option
  ; local_gateway_virtual_interface_group_id : String.t option
  ; local_gateway_virtual_interface_arn : String.t option
  ; outpost_lag_id : String.t option
  ; vlan : Integer.t option
  ; local_address : String.t option
  ; peer_address : String.t option
  ; local_bgp_asn : Integer.t option
  ; peer_bgp_asn : Integer.t option
  ; peer_bgp_asn_extended : Long.t option
  ; owner_id : String.t option
  ; tags : TagList.t
  ; configuration_state : LocalGatewayVirtualInterfaceConfigurationState.t option
  }

let make
    ?local_gateway_virtual_interface_id
    ?local_gateway_id
    ?local_gateway_virtual_interface_group_id
    ?local_gateway_virtual_interface_arn
    ?outpost_lag_id
    ?vlan
    ?local_address
    ?peer_address
    ?local_bgp_asn
    ?peer_bgp_asn
    ?peer_bgp_asn_extended
    ?owner_id
    ?(tags = [])
    ?configuration_state
    () =
  { local_gateway_virtual_interface_id
  ; local_gateway_id
  ; local_gateway_virtual_interface_group_id
  ; local_gateway_virtual_interface_arn
  ; outpost_lag_id
  ; vlan
  ; local_address
  ; peer_address
  ; local_bgp_asn
  ; peer_bgp_asn
  ; peer_bgp_asn_extended
  ; owner_id
  ; tags
  ; configuration_state
  }

let parse xml =
  Some
    { local_gateway_virtual_interface_id =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayVirtualInterfaceId" xml)
          String.parse
    ; local_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayId" xml) String.parse
    ; local_gateway_virtual_interface_group_id =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayVirtualInterfaceGroupId" xml)
          String.parse
    ; local_gateway_virtual_interface_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayVirtualInterfaceArn" xml)
          String.parse
    ; outpost_lag_id =
        Aws.Util.option_bind (Aws.Xml.member "outpostLagId" xml) String.parse
    ; vlan = Aws.Util.option_bind (Aws.Xml.member "vlan" xml) Integer.parse
    ; local_address =
        Aws.Util.option_bind (Aws.Xml.member "localAddress" xml) String.parse
    ; peer_address = Aws.Util.option_bind (Aws.Xml.member "peerAddress" xml) String.parse
    ; local_bgp_asn =
        Aws.Util.option_bind (Aws.Xml.member "localBgpAsn" xml) Integer.parse
    ; peer_bgp_asn = Aws.Util.option_bind (Aws.Xml.member "peerBgpAsn" xml) Integer.parse
    ; peer_bgp_asn_extended =
        Aws.Util.option_bind (Aws.Xml.member "peerBgpAsnExtended" xml) Long.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; configuration_state =
        Aws.Util.option_bind
          (Aws.Xml.member "configurationState" xml)
          LocalGatewayVirtualInterfaceConfigurationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configuration_state (fun f ->
             Aws.Query.Pair
               ( "ConfigurationState"
               , LocalGatewayVirtualInterfaceConfigurationState.to_query f ))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.peer_bgp_asn_extended (fun f ->
             Aws.Query.Pair ("PeerBgpAsnExtended", Long.to_query f))
       ; Aws.Util.option_map v.peer_bgp_asn (fun f ->
             Aws.Query.Pair ("PeerBgpAsn", Integer.to_query f))
       ; Aws.Util.option_map v.local_bgp_asn (fun f ->
             Aws.Query.Pair ("LocalBgpAsn", Integer.to_query f))
       ; Aws.Util.option_map v.peer_address (fun f ->
             Aws.Query.Pair ("PeerAddress", String.to_query f))
       ; Aws.Util.option_map v.local_address (fun f ->
             Aws.Query.Pair ("LocalAddress", String.to_query f))
       ; Aws.Util.option_map v.vlan (fun f -> Aws.Query.Pair ("Vlan", Integer.to_query f))
       ; Aws.Util.option_map v.outpost_lag_id (fun f ->
             Aws.Query.Pair ("OutpostLagId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_virtual_interface_arn (fun f ->
             Aws.Query.Pair ("LocalGatewayVirtualInterfaceArn", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_id (fun f ->
             Aws.Query.Pair ("LocalGatewayVirtualInterfaceGroupId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_id (fun f ->
             Aws.Query.Pair ("LocalGatewayId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_virtual_interface_id (fun f ->
             Aws.Query.Pair ("LocalGatewayVirtualInterfaceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configuration_state (fun f ->
             ( "configurationState"
             , LocalGatewayVirtualInterfaceConfigurationState.to_json f ))
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.peer_bgp_asn_extended (fun f ->
             "peerBgpAsnExtended", Long.to_json f)
       ; Aws.Util.option_map v.peer_bgp_asn (fun f -> "peerBgpAsn", Integer.to_json f)
       ; Aws.Util.option_map v.local_bgp_asn (fun f -> "localBgpAsn", Integer.to_json f)
       ; Aws.Util.option_map v.peer_address (fun f -> "peerAddress", String.to_json f)
       ; Aws.Util.option_map v.local_address (fun f -> "localAddress", String.to_json f)
       ; Aws.Util.option_map v.vlan (fun f -> "vlan", Integer.to_json f)
       ; Aws.Util.option_map v.outpost_lag_id (fun f -> "outpostLagId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_virtual_interface_arn (fun f ->
             "localGatewayVirtualInterfaceArn", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_id (fun f ->
             "localGatewayVirtualInterfaceGroupId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_id (fun f ->
             "localGatewayId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_virtual_interface_id (fun f ->
             "localGatewayVirtualInterfaceId", String.to_json f)
       ])

let of_json j =
  { local_gateway_virtual_interface_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayVirtualInterfaceId")
        String.of_json
  ; local_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayId") String.of_json
  ; local_gateway_virtual_interface_group_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayVirtualInterfaceGroupId")
        String.of_json
  ; local_gateway_virtual_interface_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayVirtualInterfaceArn")
        String.of_json
  ; outpost_lag_id = Aws.Util.option_map (Aws.Json.lookup j "outpostLagId") String.of_json
  ; vlan = Aws.Util.option_map (Aws.Json.lookup j "vlan") Integer.of_json
  ; local_address = Aws.Util.option_map (Aws.Json.lookup j "localAddress") String.of_json
  ; peer_address = Aws.Util.option_map (Aws.Json.lookup j "peerAddress") String.of_json
  ; local_bgp_asn = Aws.Util.option_map (Aws.Json.lookup j "localBgpAsn") Integer.of_json
  ; peer_bgp_asn = Aws.Util.option_map (Aws.Json.lookup j "peerBgpAsn") Integer.of_json
  ; peer_bgp_asn_extended =
      Aws.Util.option_map (Aws.Json.lookup j "peerBgpAsnExtended") Long.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; configuration_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "configurationState")
        LocalGatewayVirtualInterfaceConfigurationState.of_json
  }
