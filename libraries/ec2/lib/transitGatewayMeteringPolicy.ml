open Aws.BaseTypes

type t =
  { transit_gateway_metering_policy_id : String.t option
  ; transit_gateway_id : String.t option
  ; middlebox_attachment_ids : ValueStringList.t
  ; state : TransitGatewayMeteringPolicyState.t option
  ; update_effective_at : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?transit_gateway_metering_policy_id
    ?transit_gateway_id
    ?(middlebox_attachment_ids = [])
    ?state
    ?update_effective_at
    ?(tags = [])
    () =
  { transit_gateway_metering_policy_id
  ; transit_gateway_id
  ; middlebox_attachment_ids
  ; state
  ; update_effective_at
  ; tags
  }

let parse xml =
  Some
    { transit_gateway_metering_policy_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayMeteringPolicyId" xml)
          String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; middlebox_attachment_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "middleboxAttachmentIdSet" xml)
             ValueStringList.parse)
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayMeteringPolicyState.parse
    ; update_effective_at =
        Aws.Util.option_bind (Aws.Xml.member "updateEffectiveAt" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.update_effective_at (fun f ->
             Aws.Query.Pair ("UpdateEffectiveAt", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayMeteringPolicyState.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MiddleboxAttachmentIdSet"
              , ValueStringList.to_query v.middlebox_attachment_ids ))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_metering_policy_id (fun f ->
             Aws.Query.Pair ("TransitGatewayMeteringPolicyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.update_effective_at (fun f ->
             "updateEffectiveAt", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayMeteringPolicyState.to_json f)
       ; Some
           ("middleboxAttachmentIdSet", ValueStringList.to_json v.middlebox_attachment_ids)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_metering_policy_id (fun f ->
             "transitGatewayMeteringPolicyId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_metering_policy_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayMeteringPolicyId")
        String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; middlebox_attachment_ids =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "middleboxAttachmentIdSet"))
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayMeteringPolicyState.of_json
  ; update_effective_at =
      Aws.Util.option_map (Aws.Json.lookup j "updateEffectiveAt") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
