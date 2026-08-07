open Aws.BaseTypes

type t =
  { service_link_virtual_interface_id : String.t option
  ; service_link_virtual_interface_arn : String.t option
  ; outpost_id : String.t option
  ; outpost_arn : String.t option
  ; owner_id : String.t option
  ; local_address : String.t option
  ; peer_address : String.t option
  ; peer_bgp_asn : Long.t option
  ; vlan : Integer.t option
  ; outpost_lag_id : String.t option
  ; tags : TagList.t
  ; configuration_state : ServiceLinkVirtualInterfaceConfigurationState.t option
  }

let make
    ?service_link_virtual_interface_id
    ?service_link_virtual_interface_arn
    ?outpost_id
    ?outpost_arn
    ?owner_id
    ?local_address
    ?peer_address
    ?peer_bgp_asn
    ?vlan
    ?outpost_lag_id
    ?(tags = [])
    ?configuration_state
    () =
  { service_link_virtual_interface_id
  ; service_link_virtual_interface_arn
  ; outpost_id
  ; outpost_arn
  ; owner_id
  ; local_address
  ; peer_address
  ; peer_bgp_asn
  ; vlan
  ; outpost_lag_id
  ; tags
  ; configuration_state
  }

let parse xml =
  Some
    { service_link_virtual_interface_id =
        Aws.Util.option_bind
          (Aws.Xml.member "serviceLinkVirtualInterfaceId" xml)
          String.parse
    ; service_link_virtual_interface_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "serviceLinkVirtualInterfaceArn" xml)
          String.parse
    ; outpost_id = Aws.Util.option_bind (Aws.Xml.member "outpostId" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; local_address =
        Aws.Util.option_bind (Aws.Xml.member "localAddress" xml) String.parse
    ; peer_address = Aws.Util.option_bind (Aws.Xml.member "peerAddress" xml) String.parse
    ; peer_bgp_asn = Aws.Util.option_bind (Aws.Xml.member "peerBgpAsn" xml) Long.parse
    ; vlan = Aws.Util.option_bind (Aws.Xml.member "vlan" xml) Integer.parse
    ; outpost_lag_id =
        Aws.Util.option_bind (Aws.Xml.member "outpostLagId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; configuration_state =
        Aws.Util.option_bind
          (Aws.Xml.member "configurationState" xml)
          ServiceLinkVirtualInterfaceConfigurationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configuration_state (fun f ->
             Aws.Query.Pair
               ( "ConfigurationState"
               , ServiceLinkVirtualInterfaceConfigurationState.to_query f ))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.outpost_lag_id (fun f ->
             Aws.Query.Pair ("OutpostLagId", String.to_query f))
       ; Aws.Util.option_map v.vlan (fun f -> Aws.Query.Pair ("Vlan", Integer.to_query f))
       ; Aws.Util.option_map v.peer_bgp_asn (fun f ->
             Aws.Query.Pair ("PeerBgpAsn", Long.to_query f))
       ; Aws.Util.option_map v.peer_address (fun f ->
             Aws.Query.Pair ("PeerAddress", String.to_query f))
       ; Aws.Util.option_map v.local_address (fun f ->
             Aws.Query.Pair ("LocalAddress", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.outpost_id (fun f ->
             Aws.Query.Pair ("OutpostId", String.to_query f))
       ; Aws.Util.option_map v.service_link_virtual_interface_arn (fun f ->
             Aws.Query.Pair ("ServiceLinkVirtualInterfaceArn", String.to_query f))
       ; Aws.Util.option_map v.service_link_virtual_interface_id (fun f ->
             Aws.Query.Pair ("ServiceLinkVirtualInterfaceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configuration_state (fun f ->
             "configurationState", ServiceLinkVirtualInterfaceConfigurationState.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.outpost_lag_id (fun f -> "outpostLagId", String.to_json f)
       ; Aws.Util.option_map v.vlan (fun f -> "vlan", Integer.to_json f)
       ; Aws.Util.option_map v.peer_bgp_asn (fun f -> "peerBgpAsn", Long.to_json f)
       ; Aws.Util.option_map v.peer_address (fun f -> "peerAddress", String.to_json f)
       ; Aws.Util.option_map v.local_address (fun f -> "localAddress", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.outpost_id (fun f -> "outpostId", String.to_json f)
       ; Aws.Util.option_map v.service_link_virtual_interface_arn (fun f ->
             "serviceLinkVirtualInterfaceArn", String.to_json f)
       ; Aws.Util.option_map v.service_link_virtual_interface_id (fun f ->
             "serviceLinkVirtualInterfaceId", String.to_json f)
       ])

let of_json j =
  { service_link_virtual_interface_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "serviceLinkVirtualInterfaceId")
        String.of_json
  ; service_link_virtual_interface_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "serviceLinkVirtualInterfaceArn")
        String.of_json
  ; outpost_id = Aws.Util.option_map (Aws.Json.lookup j "outpostId") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; local_address = Aws.Util.option_map (Aws.Json.lookup j "localAddress") String.of_json
  ; peer_address = Aws.Util.option_map (Aws.Json.lookup j "peerAddress") String.of_json
  ; peer_bgp_asn = Aws.Util.option_map (Aws.Json.lookup j "peerBgpAsn") Long.of_json
  ; vlan = Aws.Util.option_map (Aws.Json.lookup j "vlan") Integer.of_json
  ; outpost_lag_id = Aws.Util.option_map (Aws.Json.lookup j "outpostLagId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; configuration_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "configurationState")
        ServiceLinkVirtualInterfaceConfigurationState.of_json
  }
