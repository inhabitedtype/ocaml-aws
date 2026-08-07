open Aws.BaseTypes

type t =
  { amazon_side_asn : Long.t option
  ; tags : TagList.t
  ; vpn_gateway_id : String.t option
  ; state : VpnState.t option
  ; type_ : GatewayType.t option
  ; availability_zone : String.t option
  ; vpc_attachments : VpcAttachmentList.t
  }

let make
    ?amazon_side_asn
    ?(tags = [])
    ?vpn_gateway_id
    ?state
    ?type_
    ?availability_zone
    ?(vpc_attachments = [])
    () =
  { amazon_side_asn
  ; tags
  ; vpn_gateway_id
  ; state
  ; type_
  ; availability_zone
  ; vpc_attachments
  }

let parse xml =
  Some
    { amazon_side_asn =
        Aws.Util.option_bind (Aws.Xml.member "amazonSideAsn" xml) Long.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; vpn_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "vpnGatewayId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) VpnState.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) GatewayType.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; vpc_attachments =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "attachments" xml)
             VpcAttachmentList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Attachments", VpcAttachmentList.to_query v.vpc_attachments))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", GatewayType.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", VpnState.to_query f))
       ; Aws.Util.option_map v.vpn_gateway_id (fun f ->
             Aws.Query.Pair ("VpnGatewayId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.amazon_side_asn (fun f ->
             Aws.Query.Pair ("AmazonSideAsn", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("attachments", VpcAttachmentList.to_json v.vpc_attachments)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", GatewayType.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", VpnState.to_json f)
       ; Aws.Util.option_map v.vpn_gateway_id (fun f -> "vpnGatewayId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.amazon_side_asn (fun f -> "amazonSideAsn", Long.to_json f)
       ])

let of_json j =
  { amazon_side_asn = Aws.Util.option_map (Aws.Json.lookup j "amazonSideAsn") Long.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vpn_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "vpnGatewayId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") VpnState.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") GatewayType.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; vpc_attachments =
      VpcAttachmentList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "attachments"))
  }
