open Aws.BaseTypes

type t =
  { local_gateway_virtual_interface_group_id : String.t
  ; outpost_lag_id : String.t
  ; vlan : Integer.t
  ; local_address : String.t
  ; peer_address : String.t
  ; peer_bgp_asn : Integer.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; peer_bgp_asn_extended : Long.t option
  }

let make
    ~local_gateway_virtual_interface_group_id
    ~outpost_lag_id
    ~vlan
    ~local_address
    ~peer_address
    ?peer_bgp_asn
    ?(tag_specifications = [])
    ?dry_run
    ?peer_bgp_asn_extended
    () =
  { local_gateway_virtual_interface_group_id
  ; outpost_lag_id
  ; vlan
  ; local_address
  ; peer_address
  ; peer_bgp_asn
  ; tag_specifications
  ; dry_run
  ; peer_bgp_asn_extended
  }

let parse xml =
  Some
    { local_gateway_virtual_interface_group_id =
        Aws.Xml.required
          "LocalGatewayVirtualInterfaceGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "LocalGatewayVirtualInterfaceGroupId" xml)
             String.parse)
    ; outpost_lag_id =
        Aws.Xml.required
          "OutpostLagId"
          (Aws.Util.option_bind (Aws.Xml.member "OutpostLagId" xml) String.parse)
    ; vlan =
        Aws.Xml.required
          "Vlan"
          (Aws.Util.option_bind (Aws.Xml.member "Vlan" xml) Integer.parse)
    ; local_address =
        Aws.Xml.required
          "LocalAddress"
          (Aws.Util.option_bind (Aws.Xml.member "LocalAddress" xml) String.parse)
    ; peer_address =
        Aws.Xml.required
          "PeerAddress"
          (Aws.Util.option_bind (Aws.Xml.member "PeerAddress" xml) String.parse)
    ; peer_bgp_asn = Aws.Util.option_bind (Aws.Xml.member "PeerBgpAsn" xml) Integer.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; peer_bgp_asn_extended =
        Aws.Util.option_bind (Aws.Xml.member "PeerBgpAsnExtended" xml) Long.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_bgp_asn_extended (fun f ->
             Aws.Query.Pair ("PeerBgpAsnExtended", Long.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.peer_bgp_asn (fun f ->
             Aws.Query.Pair ("PeerBgpAsn", Integer.to_query f))
       ; Some (Aws.Query.Pair ("PeerAddress", String.to_query v.peer_address))
       ; Some (Aws.Query.Pair ("LocalAddress", String.to_query v.local_address))
       ; Some (Aws.Query.Pair ("Vlan", Integer.to_query v.vlan))
       ; Some (Aws.Query.Pair ("OutpostLagId", String.to_query v.outpost_lag_id))
       ; Some
           (Aws.Query.Pair
              ( "LocalGatewayVirtualInterfaceGroupId"
              , String.to_query v.local_gateway_virtual_interface_group_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_bgp_asn_extended (fun f ->
             "PeerBgpAsnExtended", Long.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.peer_bgp_asn (fun f -> "PeerBgpAsn", Integer.to_json f)
       ; Some ("PeerAddress", String.to_json v.peer_address)
       ; Some ("LocalAddress", String.to_json v.local_address)
       ; Some ("Vlan", Integer.to_json v.vlan)
       ; Some ("OutpostLagId", String.to_json v.outpost_lag_id)
       ; Some
           ( "LocalGatewayVirtualInterfaceGroupId"
           , String.to_json v.local_gateway_virtual_interface_group_id )
       ])

let of_json j =
  { local_gateway_virtual_interface_group_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LocalGatewayVirtualInterfaceGroupId"))
  ; outpost_lag_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OutpostLagId"))
  ; vlan = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Vlan"))
  ; local_address =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LocalAddress"))
  ; peer_address =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PeerAddress"))
  ; peer_bgp_asn = Aws.Util.option_map (Aws.Json.lookup j "PeerBgpAsn") Integer.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; peer_bgp_asn_extended =
      Aws.Util.option_map (Aws.Json.lookup j "PeerBgpAsnExtended") Long.of_json
  }
