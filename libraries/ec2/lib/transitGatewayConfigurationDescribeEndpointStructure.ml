open Aws.BaseTypes

type t =
  { transit_gateway_id : String.t option
  ; transit_gateway_attachment_id : String.t option
  ; availability_zones : ClientVpnAvailabilityZoneSet.t
  ; availability_zone_ids : ClientVpnAvailabilityZoneIdSet.t
  }

let make
    ?transit_gateway_id
    ?transit_gateway_attachment_id
    ?(availability_zones = [])
    ?(availability_zone_ids = [])
    () =
  { transit_gateway_id
  ; transit_gateway_attachment_id
  ; availability_zones
  ; availability_zone_ids
  }

let parse xml =
  Some
    { transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; transit_gateway_attachment_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachmentId" xml)
          String.parse
    ; availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "availabilityZoneSet" xml)
             ClientVpnAvailabilityZoneSet.parse)
    ; availability_zone_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "availabilityZoneIdSet" xml)
             ClientVpnAvailabilityZoneIdSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AvailabilityZoneIdSet"
              , ClientVpnAvailabilityZoneIdSet.to_query v.availability_zone_ids ))
       ; Some
           (Aws.Query.Pair
              ( "AvailabilityZoneSet"
              , ClientVpnAvailabilityZoneSet.to_query v.availability_zones ))
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             Aws.Query.Pair ("TransitGatewayAttachmentId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "availabilityZoneIdSet"
           , ClientVpnAvailabilityZoneIdSet.to_json v.availability_zone_ids )
       ; Some
           ( "availabilityZoneSet"
           , ClientVpnAvailabilityZoneSet.to_json v.availability_zones )
       ; Aws.Util.option_map v.transit_gateway_attachment_id (fun f ->
             "transitGatewayAttachmentId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; transit_gateway_attachment_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAttachmentId") String.of_json
  ; availability_zones =
      ClientVpnAvailabilityZoneSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneSet"))
  ; availability_zone_ids =
      ClientVpnAvailabilityZoneIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZoneIdSet"))
  }
