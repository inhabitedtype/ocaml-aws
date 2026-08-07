open Aws.BaseTypes

type t =
  { vpn_concentrator_id : String.t option
  ; state : String.t option
  ; transit_gateway_id : String.t option
  ; transit_gateway_attachment_id : String.t option
  ; type_ : String.t option
  ; tags : TagList.t
  }

let make
    ?vpn_concentrator_id
    ?state
    ?transit_gateway_id
    ?transit_gateway_attachment_id
    ?type_
    ?(tags = [])
    () =
  { vpn_concentrator_id
  ; state
  ; transit_gateway_id
  ; transit_gateway_attachment_id
  ; type_
  ; tags
  }

let parse xml =
  Some
    { vpn_concentrator_id =
        Aws.Util.option_bind (Aws.Xml.member "vpnConcentratorId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.type_ (fun f -> Aws.Query.Pair ("Type", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.vpn_concentrator_id (fun f ->
             Aws.Query.Pair ("VpnConcentratorId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.type_ (fun f -> "type", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ; Aws.Util.option_map v.vpn_concentrator_id (fun f ->
             "vpnConcentratorId", String.to_json f)
       ])

let of_json j =
  { vpn_concentrator_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpnConcentratorId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
