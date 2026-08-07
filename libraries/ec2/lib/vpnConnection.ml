open Aws.BaseTypes

type t =
  { category : String.t option
  ; transit_gateway_id : String.t option
  ; vpn_concentrator_id : String.t option
  ; core_network_arn : String.t option
  ; core_network_attachment_arn : String.t option
  ; gateway_association_state : GatewayAssociationState.t option
  ; options : VpnConnectionOptions.t option
  ; routes : VpnStaticRouteList.t
  ; tags : TagList.t
  ; vgw_telemetry : VgwTelemetryList.t
  ; pre_shared_key_arn : String.t option
  ; vpn_connection_id : String.t
  ; state : VpnState.t
  ; customer_gateway_configuration : String.t option
  ; type_ : GatewayType.t
  ; customer_gateway_id : String.t
  ; vpn_gateway_id : String.t option
  }

let make
    ?category
    ?transit_gateway_id
    ?vpn_concentrator_id
    ?core_network_arn
    ?core_network_attachment_arn
    ?gateway_association_state
    ?options
    ?(routes = [])
    ?(tags = [])
    ?(vgw_telemetry = [])
    ?pre_shared_key_arn
    ~vpn_connection_id
    ~state
    ?customer_gateway_configuration
    ~type_
    ~customer_gateway_id
    ?vpn_gateway_id
    () =
  { category
  ; transit_gateway_id
  ; vpn_concentrator_id
  ; core_network_arn
  ; core_network_attachment_arn
  ; gateway_association_state
  ; options
  ; routes
  ; tags
  ; vgw_telemetry
  ; pre_shared_key_arn
  ; vpn_connection_id
  ; state
  ; customer_gateway_configuration
  ; type_
  ; customer_gateway_id
  ; vpn_gateway_id
  }

let parse xml =
  Some
    { category = Aws.Util.option_bind (Aws.Xml.member "category" xml) String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; vpn_concentrator_id =
        Aws.Util.option_bind (Aws.Xml.member "vpnConcentratorId" xml) String.parse
    ; core_network_arn =
        Aws.Util.option_bind (Aws.Xml.member "coreNetworkArn" xml) String.parse
    ; core_network_attachment_arn =
        Aws.Util.option_bind (Aws.Xml.member "coreNetworkAttachmentArn" xml) String.parse
    ; gateway_association_state =
        Aws.Util.option_bind
          (Aws.Xml.member "gatewayAssociationState" xml)
          GatewayAssociationState.parse
    ; options =
        Aws.Util.option_bind (Aws.Xml.member "options" xml) VpnConnectionOptions.parse
    ; routes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "routes" xml) VpnStaticRouteList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; vgw_telemetry =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "vgwTelemetry" xml)
             VgwTelemetryList.parse)
    ; pre_shared_key_arn =
        Aws.Util.option_bind (Aws.Xml.member "preSharedKeyArn" xml) String.parse
    ; vpn_connection_id =
        Aws.Xml.required
          "vpnConnectionId"
          (Aws.Util.option_bind (Aws.Xml.member "vpnConnectionId" xml) String.parse)
    ; state =
        Aws.Xml.required
          "state"
          (Aws.Util.option_bind (Aws.Xml.member "state" xml) VpnState.parse)
    ; customer_gateway_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "customerGatewayConfiguration" xml)
          String.parse
    ; type_ =
        Aws.Xml.required
          "type"
          (Aws.Util.option_bind (Aws.Xml.member "type" xml) GatewayType.parse)
    ; customer_gateway_id =
        Aws.Xml.required
          "customerGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "customerGatewayId" xml) String.parse)
    ; vpn_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "vpnGatewayId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_gateway_id (fun f ->
             Aws.Query.Pair ("VpnGatewayId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("CustomerGatewayId", String.to_query v.customer_gateway_id))
       ; Some (Aws.Query.Pair ("Type", GatewayType.to_query v.type_))
       ; Aws.Util.option_map v.customer_gateway_configuration (fun f ->
             Aws.Query.Pair ("CustomerGatewayConfiguration", String.to_query f))
       ; Some (Aws.Query.Pair ("State", VpnState.to_query v.state))
       ; Some (Aws.Query.Pair ("VpnConnectionId", String.to_query v.vpn_connection_id))
       ; Aws.Util.option_map v.pre_shared_key_arn (fun f ->
             Aws.Query.Pair ("PreSharedKeyArn", String.to_query f))
       ; Some (Aws.Query.Pair ("VgwTelemetry", VgwTelemetryList.to_query v.vgw_telemetry))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("Routes", VpnStaticRouteList.to_query v.routes))
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair ("Options", VpnConnectionOptions.to_query f))
       ; Aws.Util.option_map v.gateway_association_state (fun f ->
             Aws.Query.Pair ("GatewayAssociationState", GatewayAssociationState.to_query f))
       ; Aws.Util.option_map v.core_network_attachment_arn (fun f ->
             Aws.Query.Pair ("CoreNetworkAttachmentArn", String.to_query f))
       ; Aws.Util.option_map v.core_network_arn (fun f ->
             Aws.Query.Pair ("CoreNetworkArn", String.to_query f))
       ; Aws.Util.option_map v.vpn_concentrator_id (fun f ->
             Aws.Query.Pair ("VpnConcentratorId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.category (fun f ->
             Aws.Query.Pair ("Category", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpn_gateway_id (fun f -> "vpnGatewayId", String.to_json f)
       ; Some ("customerGatewayId", String.to_json v.customer_gateway_id)
       ; Some ("type", GatewayType.to_json v.type_)
       ; Aws.Util.option_map v.customer_gateway_configuration (fun f ->
             "customerGatewayConfiguration", String.to_json f)
       ; Some ("state", VpnState.to_json v.state)
       ; Some ("vpnConnectionId", String.to_json v.vpn_connection_id)
       ; Aws.Util.option_map v.pre_shared_key_arn (fun f ->
             "preSharedKeyArn", String.to_json f)
       ; Some ("vgwTelemetry", VgwTelemetryList.to_json v.vgw_telemetry)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Some ("routes", VpnStaticRouteList.to_json v.routes)
       ; Aws.Util.option_map v.options (fun f ->
             "options", VpnConnectionOptions.to_json f)
       ; Aws.Util.option_map v.gateway_association_state (fun f ->
             "gatewayAssociationState", GatewayAssociationState.to_json f)
       ; Aws.Util.option_map v.core_network_attachment_arn (fun f ->
             "coreNetworkAttachmentArn", String.to_json f)
       ; Aws.Util.option_map v.core_network_arn (fun f ->
             "coreNetworkArn", String.to_json f)
       ; Aws.Util.option_map v.vpn_concentrator_id (fun f ->
             "vpnConcentratorId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.category (fun f -> "category", String.to_json f)
       ])

let of_json j =
  { category = Aws.Util.option_map (Aws.Json.lookup j "category") String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; vpn_concentrator_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpnConcentratorId") String.of_json
  ; core_network_arn =
      Aws.Util.option_map (Aws.Json.lookup j "coreNetworkArn") String.of_json
  ; core_network_attachment_arn =
      Aws.Util.option_map (Aws.Json.lookup j "coreNetworkAttachmentArn") String.of_json
  ; gateway_association_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "gatewayAssociationState")
        GatewayAssociationState.of_json
  ; options =
      Aws.Util.option_map (Aws.Json.lookup j "options") VpnConnectionOptions.of_json
  ; routes =
      VpnStaticRouteList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "routes"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vgw_telemetry =
      VgwTelemetryList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vgwTelemetry"))
  ; pre_shared_key_arn =
      Aws.Util.option_map (Aws.Json.lookup j "preSharedKeyArn") String.of_json
  ; vpn_connection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpnConnectionId"))
  ; state = VpnState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "state"))
  ; customer_gateway_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "customerGatewayConfiguration")
        String.of_json
  ; type_ = GatewayType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "type"))
  ; customer_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "customerGatewayId"))
  ; vpn_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "vpnGatewayId") String.of_json
  }
