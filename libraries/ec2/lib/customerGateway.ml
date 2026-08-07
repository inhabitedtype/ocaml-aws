open Aws.BaseTypes

type t =
  { certificate_arn : String.t option
  ; device_name : String.t option
  ; tags : TagList.t
  ; bgp_asn_extended : String.t option
  ; customer_gateway_id : String.t
  ; state : String.t
  ; type_ : String.t
  ; ip_address : String.t
  ; bgp_asn : String.t
  }

let make
    ?certificate_arn
    ?device_name
    ?(tags = [])
    ?bgp_asn_extended
    ~customer_gateway_id
    ~state
    ~type_
    ~ip_address
    ~bgp_asn
    () =
  { certificate_arn
  ; device_name
  ; tags
  ; bgp_asn_extended
  ; customer_gateway_id
  ; state
  ; type_
  ; ip_address
  ; bgp_asn
  }

let parse xml =
  Some
    { certificate_arn =
        Aws.Util.option_bind (Aws.Xml.member "certificateArn" xml) String.parse
    ; device_name = Aws.Util.option_bind (Aws.Xml.member "deviceName" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; bgp_asn_extended =
        Aws.Util.option_bind (Aws.Xml.member "bgpAsnExtended" xml) String.parse
    ; customer_gateway_id =
        Aws.Xml.required
          "customerGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "customerGatewayId" xml) String.parse)
    ; state =
        Aws.Xml.required
          "state"
          (Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse)
    ; type_ =
        Aws.Xml.required
          "type"
          (Aws.Util.option_bind (Aws.Xml.member "type" xml) String.parse)
    ; ip_address =
        Aws.Xml.required
          "ipAddress"
          (Aws.Util.option_bind (Aws.Xml.member "ipAddress" xml) String.parse)
    ; bgp_asn =
        Aws.Xml.required
          "bgpAsn"
          (Aws.Util.option_bind (Aws.Xml.member "bgpAsn" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("BgpAsn", String.to_query v.bgp_asn))
       ; Some (Aws.Query.Pair ("IpAddress", String.to_query v.ip_address))
       ; Some (Aws.Query.Pair ("Type", String.to_query v.type_))
       ; Some (Aws.Query.Pair ("State", String.to_query v.state))
       ; Some
           (Aws.Query.Pair ("CustomerGatewayId", String.to_query v.customer_gateway_id))
       ; Aws.Util.option_map v.bgp_asn_extended (fun f ->
             Aws.Query.Pair ("BgpAsnExtended", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.device_name (fun f ->
             Aws.Query.Pair ("DeviceName", String.to_query f))
       ; Aws.Util.option_map v.certificate_arn (fun f ->
             Aws.Query.Pair ("CertificateArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("bgpAsn", String.to_json v.bgp_asn)
       ; Some ("ipAddress", String.to_json v.ip_address)
       ; Some ("type", String.to_json v.type_)
       ; Some ("state", String.to_json v.state)
       ; Some ("customerGatewayId", String.to_json v.customer_gateway_id)
       ; Aws.Util.option_map v.bgp_asn_extended (fun f ->
             "bgpAsnExtended", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.device_name (fun f -> "deviceName", String.to_json f)
       ; Aws.Util.option_map v.certificate_arn (fun f ->
             "certificateArn", String.to_json f)
       ])

let of_json j =
  { certificate_arn =
      Aws.Util.option_map (Aws.Json.lookup j "certificateArn") String.of_json
  ; device_name = Aws.Util.option_map (Aws.Json.lookup j "deviceName") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; bgp_asn_extended =
      Aws.Util.option_map (Aws.Json.lookup j "bgpAsnExtended") String.of_json
  ; customer_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "customerGatewayId"))
  ; state = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "state"))
  ; type_ = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "type"))
  ; ip_address = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipAddress"))
  ; bgp_asn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "bgpAsn"))
  }
