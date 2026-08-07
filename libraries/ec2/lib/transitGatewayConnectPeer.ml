open Aws.BaseTypes

type t =
  { transit_gateway_attachment_id : String.t option
  ; transit_gateway_connect_peer_id : String.t option
  ; state : TransitGatewayConnectPeerState.t option
  ; creation_time : DateTime.t option
  ; connect_peer_configuration : TransitGatewayConnectPeerConfiguration.t option
  ; tags : TagList.t
  }

let make
    ?transit_gateway_attachment_id
    ?transit_gateway_connect_peer_id
    ?state
    ?creation_time
    ?connect_peer_configuration
    ?(tags = [])
    () =
  { transit_gateway_attachment_id
  ; transit_gateway_connect_peer_id
  ; state
  ; creation_time
  ; connect_peer_configuration
  ; tags
  }

let parse xml =
  Some
    { transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; transit_gateway_connect_peer_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayConnectPeerId" xml)
          String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayConnectPeerState.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) DateTime.parse
    ; connect_peer_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "connectPeerConfiguration" xml)
          TransitGatewayConnectPeerConfiguration.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.connect_peer_configuration (fun f ->
             Aws.Query.Pair
               ( "ConnectPeerConfiguration"
               , TransitGatewayConnectPeerConfiguration.to_query f ))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayConnectPeerState.to_query f))
       ; Aws.Util.option_map v.transit_gateway_connect_peer_id (fun f ->
             Aws.Query.Pair ("TransitGatewayConnectPeerId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.connect_peer_configuration (fun f ->
             "connectPeerConfiguration", TransitGatewayConnectPeerConfiguration.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayConnectPeerState.to_json f)
       ; Aws.Util.option_map v.transit_gateway_connect_peer_id (fun f ->
             "transitGatewayConnectPeerId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  ; transit_gateway_connect_peer_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayConnectPeerId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayConnectPeerState.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; connect_peer_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "connectPeerConfiguration")
        TransitGatewayConnectPeerConfiguration.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
