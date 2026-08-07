open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t option
  ; destination_cidr : String.t option
  ; target_subnet : String.t option
  ; type_ : String.t option
  ; origin : String.t option
  ; status : ClientVpnRouteStatus.t option
  ; description : String.t option
  ; transit_gateway_attachment_id : String.t option
  }

let make
    ?client_vpn_endpoint_id
    ?destination_cidr
    ?target_subnet
    ?type_
    ?origin
    ?status
    ?description
    ?transit_gateway_attachment_id
    () =
  { client_vpn_endpoint_id
  ; destination_cidr
  ; target_subnet
  ; type_
  ; origin
  ; status
  ; description
  ; transit_gateway_attachment_id
  }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "clientVpnEndpointId" xml) String.parse
    ; destination_cidr =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidr" xml) String.parse
    ; target_subnet =
        Aws.Util.option_bind (Aws.Xml.member "targetSubnet" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) String.parse
    ; origin = Aws.Util.option_bind (Aws.Xml.member "origin" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) ClientVpnRouteStatus.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientVpnRouteStatus.to_query f))
       ; Aws.Util.option_map v.origin (fun f ->
             Aws.Query.Pair ("Origin", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f -> Aws.Query.Pair ("Type", String.to_query f))
       ; Aws.Util.option_map v.target_subnet (fun f ->
             Aws.Query.Pair ("TargetSubnet", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr (fun f ->
             Aws.Query.Pair ("DestinationCidr", String.to_query f))
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             Aws.Query.Pair ("ClientVpnEndpointId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", ClientVpnRouteStatus.to_json f)
       ; Aws.Util.option_map v.origin (fun f -> "origin", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", String.to_json f)
       ; Aws.Util.option_map v.target_subnet (fun f -> "targetSubnet", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr (fun f ->
             "destinationCidr", String.to_json f)
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             "clientVpnEndpointId", String.to_json f)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "clientVpnEndpointId") String.of_json
  ; destination_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidr") String.of_json
  ; target_subnet = Aws.Util.option_map (Aws.Json.lookup j "targetSubnet") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") String.of_json
  ; origin = Aws.Util.option_map (Aws.Json.lookup j "origin") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") ClientVpnRouteStatus.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  }
