open Aws.BaseTypes

type t =
  { transit_gateway_attachment_id : String.t option
  ; accepter_transit_gateway_attachment_id : String.t option
  ; requester_tgw_info : PeeringTgwInfo.t option
  ; accepter_tgw_info : PeeringTgwInfo.t option
  ; options : TransitGatewayPeeringAttachmentOptions.t option
  ; status : PeeringAttachmentStatus.t option
  ; state : TransitGatewayAttachmentState.t option
  ; creation_time : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?transit_gateway_attachment_id
    ?accepter_transit_gateway_attachment_id
    ?requester_tgw_info
    ?accepter_tgw_info
    ?options
    ?status
    ?state
    ?creation_time
    ?(tags = [])
    () =
  { transit_gateway_attachment_id
  ; accepter_transit_gateway_attachment_id
  ; requester_tgw_info
  ; accepter_tgw_info
  ; options
  ; status
  ; state
  ; creation_time
  ; tags
  }

let parse xml =
  Some
    { transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; accepter_transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "accepterTransitGatewayAttachmentId" xml)
          String.parse
    ; requester_tgw_info =
        Aws.Util.option_bind (Aws.Xml.member "requesterTgwInfo" xml) PeeringTgwInfo.parse
    ; accepter_tgw_info =
        Aws.Util.option_bind (Aws.Xml.member "accepterTgwInfo" xml) PeeringTgwInfo.parse
    ; options =
        Aws.Util.option_bind
          (Aws.Xml.member "options" xml)
          TransitGatewayPeeringAttachmentOptions.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) PeeringAttachmentStatus.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayAttachmentState.parse
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
             Aws.Query.Pair ("State", TransitGatewayAttachmentState.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", PeeringAttachmentStatus.to_query f))
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair ("Options", TransitGatewayPeeringAttachmentOptions.to_query f))
       ; Aws.Util.option_map v.accepter_tgw_info (fun f ->
             Aws.Query.Pair ("AccepterTgwInfo", PeeringTgwInfo.to_query f))
       ; Aws.Util.option_map v.requester_tgw_info (fun f ->
             Aws.Query.Pair ("RequesterTgwInfo", PeeringTgwInfo.to_query f))
       ; Aws.Util.option_map v.accepter_transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("AccepterTransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayAttachmentState.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", PeeringAttachmentStatus.to_json f)
       ; Aws.Util.option_map v.options (fun f ->
             "options", TransitGatewayPeeringAttachmentOptions.to_json f)
       ; Aws.Util.option_map v.accepter_tgw_info (fun f ->
             "accepterTgwInfo", PeeringTgwInfo.to_json f)
       ; Aws.Util.option_map v.requester_tgw_info (fun f ->
             "requesterTgwInfo", PeeringTgwInfo.to_json f)
       ; Aws.Util.option_map v.accepter_transit_gateway_attachment_id (fun f ->
             "accepterTransitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  ; accepter_transit_gateway_attachment_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "accepterTransitGatewayAttachmentId")
        String.of_json
  ; requester_tgw_info =
      Aws.Util.option_map (Aws.Json.lookup j "requesterTgwInfo") PeeringTgwInfo.of_json
  ; accepter_tgw_info =
      Aws.Util.option_map (Aws.Json.lookup j "accepterTgwInfo") PeeringTgwInfo.of_json
  ; options =
      Aws.Util.option_map
        (Aws.Json.lookup j "options")
        TransitGatewayPeeringAttachmentOptions.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") PeeringAttachmentStatus.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayAttachmentState.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
