open Aws.BaseTypes

type t =
  { transit_gateway_attachment_id : String.t option
  ; transit_gateway_id : String.t option
  ; transit_gateway_owner_id : String.t option
  ; resource_owner_id : String.t option
  ; resource_type : TransitGatewayAttachmentResourceType.t option
  ; resource_id : String.t option
  ; state : TransitGatewayAttachmentState.t option
  ; association : TransitGatewayAttachmentAssociation.t option
  ; creation_time : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?transit_gateway_attachment_id
    ?transit_gateway_id
    ?transit_gateway_owner_id
    ?resource_owner_id
    ?resource_type
    ?resource_id
    ?state
    ?association
    ?creation_time
    ?(tags = [])
    () =
  { transit_gateway_attachment_id
  ; transit_gateway_id
  ; transit_gateway_owner_id
  ; resource_owner_id
  ; resource_type
  ; resource_id
  ; state
  ; association
  ; creation_time
  ; tags
  }

let parse xml =
  Some
    { transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; transit_gateway_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayOwnerId" xml) String.parse
    ; resource_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "resourceOwnerId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceType" xml)
          TransitGatewayAttachmentResourceType.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayAttachmentState.parse
    ; association =
        Aws.Util.option_bind
          (Aws.Xml.member "association" xml)
          TransitGatewayAttachmentAssociation.parse
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
       ; Aws.Util.option_map v.association (fun f ->
             Aws.Query.Pair ("Association", TransitGatewayAttachmentAssociation.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayAttachmentState.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair
               ("ResourceType", TransitGatewayAttachmentResourceType.to_query f))
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             Aws.Query.Pair ("ResourceOwnerId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_owner_id (fun f ->
             Aws.Query.Pair ("TransitGatewayOwnerId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.association (fun f ->
             "association", TransitGatewayAttachmentAssociation.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayAttachmentState.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", TransitGatewayAttachmentResourceType.to_json f)
       ; Aws.Util.option_map v.resource_owner_id (fun f ->
             "resourceOwnerId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_owner_id (fun f ->
             "transitGatewayOwnerId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; transit_gateway_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayOwnerId") String.of_json
  ; resource_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "resourceOwnerId") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceType")
        TransitGatewayAttachmentResourceType.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayAttachmentState.of_json
  ; association =
      Aws.Util.option_map
        (Aws.Json.lookup j "association")
        TransitGatewayAttachmentAssociation.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
