open Aws.BaseTypes

type t =
  { transit_gateway_attachment_id : String.t option
  ; resource_id : String.t option
  ; resource_type : TransitGatewayAttachmentResourceType.t option
  ; state : TransitGatewayPropagationState.t option
  ; transit_gateway_route_table_announcement_id : String.t option
  }

let make
    ?transit_gateway_attachment_id
    ?resource_id
    ?resource_type
    ?state
    ?transit_gateway_route_table_announcement_id
    () =
  { transit_gateway_attachment_id
  ; resource_id
  ; resource_type
  ; state
  ; transit_gateway_route_table_announcement_id
  }

let parse xml =
  Some
    { transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceType" xml)
          TransitGatewayAttachmentResourceType.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayPropagationState.parse
    ; transit_gateway_route_table_announcement_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableAnnouncementId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_route_table_announcement_id (fun f ->
             Aws.Query.Pair ("TransitGatewayRouteTableAnnouncementId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayPropagationState.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair
               ("ResourceType", TransitGatewayAttachmentResourceType.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_route_table_announcement_id (fun f ->
             "transitGatewayRouteTableAnnouncementId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayPropagationState.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", TransitGatewayAttachmentResourceType.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceType")
        TransitGatewayAttachmentResourceType.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayPropagationState.of_json
  ; transit_gateway_route_table_announcement_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayRouteTableAnnouncementId")
        String.of_json
  }
