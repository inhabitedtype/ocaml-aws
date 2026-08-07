open Aws.BaseTypes

type t =
  { transit_gateway_attachment_id : String.t option
  ; transit_gateway_id : String.t option
  ; client_vpn_endpoint_id : String.t option
  ; client_vpn_owner_id : String.t option
  ; state : TransitGatewayAttachmentStatusType.t option
  ; creation_time : String.t option
  }

let make
    ?transit_gateway_attachment_id
    ?transit_gateway_id
    ?client_vpn_endpoint_id
    ?client_vpn_owner_id
    ?state
    ?creation_time
    () =
  { transit_gateway_attachment_id
  ; transit_gateway_id
  ; client_vpn_endpoint_id
  ; client_vpn_owner_id
  ; state
  ; creation_time
  }

let parse xml =
  Some
    { transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; client_vpn_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "clientVpnEndpointId" xml) String.parse
    ; client_vpn_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "clientVpnOwnerId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayAttachmentStatusType.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayAttachmentStatusType.to_query f))
       ; Aws.Util.option_map v.client_vpn_owner_id (fun f ->
             Aws.Query.Pair ("ClientVpnOwnerId", String.to_query f))
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             Aws.Query.Pair ("ClientVpnEndpointId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.creation_time (fun f -> "creationTime", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayAttachmentStatusType.to_json f)
       ; Aws.Util.option_map v.client_vpn_owner_id (fun f ->
             "clientVpnOwnerId", String.to_json f)
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             "clientVpnEndpointId", String.to_json f)
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
  ; client_vpn_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "clientVpnEndpointId") String.of_json
  ; client_vpn_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "clientVpnOwnerId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayAttachmentStatusType.of_json
  ; creation_time = Aws.Util.option_map (Aws.Json.lookup j "creationTime") String.of_json
  }
