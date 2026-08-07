open Aws.BaseTypes

type t =
  { destination_cidr_block : String.t option
  ; prefix_list_id : String.t option
  ; transit_gateway_route_table_announcement_id : String.t option
  ; transit_gateway_attachments : TransitGatewayRouteAttachmentList.t
  ; type_ : TransitGatewayRouteType.t option
  ; state : TransitGatewayRouteState.t option
  }

let make
    ?destination_cidr_block
    ?prefix_list_id
    ?transit_gateway_route_table_announcement_id
    ?(transit_gateway_attachments = [])
    ?type_
    ?state
    () =
  { destination_cidr_block
  ; prefix_list_id
  ; transit_gateway_route_table_announcement_id
  ; transit_gateway_attachments
  ; type_
  ; state
  }

let parse xml =
  Some
    { destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidrBlock" xml) String.parse
    ; prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "prefixListId" xml) String.parse
    ; transit_gateway_route_table_announcement_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableAnnouncementId" xml)
          String.parse
    ; transit_gateway_attachments =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "transitGatewayAttachments" xml)
             TransitGatewayRouteAttachmentList.parse)
    ; type_ =
        Aws.Util.option_bind (Aws.Xml.member "type" xml) TransitGatewayRouteType.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) TransitGatewayRouteState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayRouteState.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", TransitGatewayRouteType.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayAttachments"
              , TransitGatewayRouteAttachmentList.to_query v.transit_gateway_attachments
              ))
       ; Aws.Util.option_map v.transit_gateway_route_table_announcement_id (fun f ->
             Aws.Query.Pair ("TransitGatewayRouteTableAnnouncementId", String.to_query f))
       ; Aws.Util.option_map v.prefix_list_id (fun f ->
             Aws.Query.Pair ("PrefixListId", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayRouteState.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", TransitGatewayRouteType.to_json f)
       ; Some
           ( "transitGatewayAttachments"
           , TransitGatewayRouteAttachmentList.to_json v.transit_gateway_attachments )
       ; Aws.Util.option_map v.transit_gateway_route_table_announcement_id (fun f ->
             "transitGatewayRouteTableAnnouncementId", String.to_json f)
       ; Aws.Util.option_map v.prefix_list_id (fun f -> "prefixListId", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "destinationCidrBlock", String.to_json f)
       ])

let of_json j =
  { destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidrBlock") String.of_json
  ; prefix_list_id = Aws.Util.option_map (Aws.Json.lookup j "prefixListId") String.of_json
  ; transit_gateway_route_table_announcement_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayRouteTableAnnouncementId")
        String.of_json
  ; transit_gateway_attachments =
      TransitGatewayRouteAttachmentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "transitGatewayAttachments"))
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") TransitGatewayRouteType.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") TransitGatewayRouteState.of_json
  }
