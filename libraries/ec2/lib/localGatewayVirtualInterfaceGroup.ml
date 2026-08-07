open Aws.BaseTypes

type t =
  { local_gateway_virtual_interface_group_id : String.t option
  ; local_gateway_virtual_interface_ids : LocalGatewayVirtualInterfaceIdSet.t
  ; local_gateway_id : String.t option
  ; owner_id : String.t option
  ; local_bgp_asn : Integer.t option
  ; local_bgp_asn_extended : Long.t option
  ; local_gateway_virtual_interface_group_arn : String.t option
  ; tags : TagList.t
  ; configuration_state : LocalGatewayVirtualInterfaceGroupConfigurationState.t option
  }

let make
    ?local_gateway_virtual_interface_group_id
    ?(local_gateway_virtual_interface_ids = [])
    ?local_gateway_id
    ?owner_id
    ?local_bgp_asn
    ?local_bgp_asn_extended
    ?local_gateway_virtual_interface_group_arn
    ?(tags = [])
    ?configuration_state
    () =
  { local_gateway_virtual_interface_group_id
  ; local_gateway_virtual_interface_ids
  ; local_gateway_id
  ; owner_id
  ; local_bgp_asn
  ; local_bgp_asn_extended
  ; local_gateway_virtual_interface_group_arn
  ; tags
  ; configuration_state
  }

let parse xml =
  Some
    { local_gateway_virtual_interface_group_id =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayVirtualInterfaceGroupId" xml)
          String.parse
    ; local_gateway_virtual_interface_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "localGatewayVirtualInterfaceIdSet" xml)
             LocalGatewayVirtualInterfaceIdSet.parse)
    ; local_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; local_bgp_asn =
        Aws.Util.option_bind (Aws.Xml.member "localBgpAsn" xml) Integer.parse
    ; local_bgp_asn_extended =
        Aws.Util.option_bind (Aws.Xml.member "localBgpAsnExtended" xml) Long.parse
    ; local_gateway_virtual_interface_group_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayVirtualInterfaceGroupArn" xml)
          String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; configuration_state =
        Aws.Util.option_bind
          (Aws.Xml.member "configurationState" xml)
          LocalGatewayVirtualInterfaceGroupConfigurationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configuration_state (fun f ->
             Aws.Query.Pair
               ( "ConfigurationState"
               , LocalGatewayVirtualInterfaceGroupConfigurationState.to_query f ))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_arn (fun f ->
             Aws.Query.Pair ("LocalGatewayVirtualInterfaceGroupArn", String.to_query f))
       ; Aws.Util.option_map v.local_bgp_asn_extended (fun f ->
             Aws.Query.Pair ("LocalBgpAsnExtended", Long.to_query f))
       ; Aws.Util.option_map v.local_bgp_asn (fun f ->
             Aws.Query.Pair ("LocalBgpAsn", Integer.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_id (fun f ->
             Aws.Query.Pair ("LocalGatewayId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LocalGatewayVirtualInterfaceIdSet"
              , LocalGatewayVirtualInterfaceIdSet.to_query
                  v.local_gateway_virtual_interface_ids ))
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_id (fun f ->
             Aws.Query.Pair ("LocalGatewayVirtualInterfaceGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configuration_state (fun f ->
             ( "configurationState"
             , LocalGatewayVirtualInterfaceGroupConfigurationState.to_json f ))
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_arn (fun f ->
             "localGatewayVirtualInterfaceGroupArn", String.to_json f)
       ; Aws.Util.option_map v.local_bgp_asn_extended (fun f ->
             "localBgpAsnExtended", Long.to_json f)
       ; Aws.Util.option_map v.local_bgp_asn (fun f -> "localBgpAsn", Integer.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_id (fun f ->
             "localGatewayId", String.to_json f)
       ; Some
           ( "localGatewayVirtualInterfaceIdSet"
           , LocalGatewayVirtualInterfaceIdSet.to_json
               v.local_gateway_virtual_interface_ids )
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_id (fun f ->
             "localGatewayVirtualInterfaceGroupId", String.to_json f)
       ])

let of_json j =
  { local_gateway_virtual_interface_group_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayVirtualInterfaceGroupId")
        String.of_json
  ; local_gateway_virtual_interface_ids =
      LocalGatewayVirtualInterfaceIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "localGatewayVirtualInterfaceIdSet"))
  ; local_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; local_bgp_asn = Aws.Util.option_map (Aws.Json.lookup j "localBgpAsn") Integer.of_json
  ; local_bgp_asn_extended =
      Aws.Util.option_map (Aws.Json.lookup j "localBgpAsnExtended") Long.of_json
  ; local_gateway_virtual_interface_group_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayVirtualInterfaceGroupArn")
        String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; configuration_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "configurationState")
        LocalGatewayVirtualInterfaceGroupConfigurationState.of_json
  }
