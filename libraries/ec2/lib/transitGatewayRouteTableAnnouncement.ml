open Aws.BaseTypes

type t =
  { transit_gateway_route_table_announcement_id : String.t option
  ; transit_gateway_id : String.t option
  ; core_network_id : String.t option
  ; peer_transit_gateway_id : String.t option
  ; peer_core_network_id : String.t option
  ; peering_attachment_id : String.t option
  ; announcement_direction : TransitGatewayRouteTableAnnouncementDirection.t option
  ; transit_gateway_route_table_id : String.t option
  ; state : TransitGatewayRouteTableAnnouncementState.t option
  ; creation_time : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?transit_gateway_route_table_announcement_id
    ?transit_gateway_id
    ?core_network_id
    ?peer_transit_gateway_id
    ?peer_core_network_id
    ?peering_attachment_id
    ?announcement_direction
    ?transit_gateway_route_table_id
    ?state
    ?creation_time
    ?(tags = [])
    () =
  { transit_gateway_route_table_announcement_id
  ; transit_gateway_id
  ; core_network_id
  ; peer_transit_gateway_id
  ; peer_core_network_id
  ; peering_attachment_id
  ; announcement_direction
  ; transit_gateway_route_table_id
  ; state
  ; creation_time
  ; tags
  }

let parse xml =
  Some
    { transit_gateway_route_table_announcement_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableAnnouncementId" xml)
          String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; core_network_id =
        Aws.Util.option_bind (Aws.Xml.member "coreNetworkId" xml) String.parse
    ; peer_transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "peerTransitGatewayId" xml) String.parse
    ; peer_core_network_id =
        Aws.Util.option_bind (Aws.Xml.member "peerCoreNetworkId" xml) String.parse
    ; peering_attachment_id =
        Aws.Util.option_bind (Aws.Xml.member "peeringAttachmentId" xml) String.parse
    ; announcement_direction =
        Aws.Util.option_bind
          (Aws.Xml.member "announcementDirection" xml)
          TransitGatewayRouteTableAnnouncementDirection.parse
    ; transit_gateway_route_table_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableId" xml)
          String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayRouteTableAnnouncementState.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayRouteTableAnnouncementState.to_query f))
       ; Aws.Util.option_map v.transit_gateway_route_table_id (fun f ->
             Aws.Query.Pair ("TransitGatewayRouteTableId", String.to_query f))
       ; Aws.Util.option_map v.announcement_direction (fun f ->
             Aws.Query.Pair
               ( "AnnouncementDirection"
               , TransitGatewayRouteTableAnnouncementDirection.to_query f ))
       ; Aws.Util.option_map v.peering_attachment_id (fun f ->
             Aws.Query.Pair ("PeeringAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.peer_core_network_id (fun f ->
             Aws.Query.Pair ("PeerCoreNetworkId", String.to_query f))
       ; Aws.Util.option_map v.peer_transit_gateway_id (fun f ->
             Aws.Query.Pair ("PeerTransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.core_network_id (fun f ->
             Aws.Query.Pair ("CoreNetworkId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_route_table_announcement_id (fun f ->
             Aws.Query.Pair ("TransitGatewayRouteTableAnnouncementId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayRouteTableAnnouncementState.to_json f)
       ; Aws.Util.option_map v.transit_gateway_route_table_id (fun f ->
             "transitGatewayRouteTableId", String.to_json f)
       ; Aws.Util.option_map v.announcement_direction (fun f ->
             ( "announcementDirection"
             , TransitGatewayRouteTableAnnouncementDirection.to_json f ))
       ; Aws.Util.option_map v.peering_attachment_id (fun f ->
             "peeringAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.peer_core_network_id (fun f ->
             "peerCoreNetworkId", String.to_json f)
       ; Aws.Util.option_map v.peer_transit_gateway_id (fun f ->
             "peerTransitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.core_network_id (fun f ->
             "coreNetworkId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_route_table_announcement_id (fun f ->
             "transitGatewayRouteTableAnnouncementId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_route_table_announcement_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayRouteTableAnnouncementId")
        String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; core_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "coreNetworkId") String.of_json
  ; peer_transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "peerTransitGatewayId") String.of_json
  ; peer_core_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "peerCoreNetworkId") String.of_json
  ; peering_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "peeringAttachmentId") String.of_json
  ; announcement_direction =
      Aws.Util.option_map
        (Aws.Json.lookup j "announcementDirection")
        TransitGatewayRouteTableAnnouncementDirection.of_json
  ; transit_gateway_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayRouteTableId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayRouteTableAnnouncementState.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
