open Aws.BaseTypes

type t =
  { bgp_asn : Integer.t option
  ; public_ip : String.t option
  ; certificate_arn : String.t option
  ; type_ : GatewayType.t
  ; tag_specifications : TagSpecificationList.t
  ; device_name : String.t option
  ; ip_address : String.t option
  ; bgp_asn_extended : Long.t option
  ; dry_run : Boolean.t option
  }

let make
    ?bgp_asn
    ?public_ip
    ?certificate_arn
    ~type_
    ?(tag_specifications = [])
    ?device_name
    ?ip_address
    ?bgp_asn_extended
    ?dry_run
    () =
  { bgp_asn
  ; public_ip
  ; certificate_arn
  ; type_
  ; tag_specifications
  ; device_name
  ; ip_address
  ; bgp_asn_extended
  ; dry_run
  }

let parse xml =
  Some
    { bgp_asn = Aws.Util.option_bind (Aws.Xml.member "BgpAsn" xml) Integer.parse
    ; public_ip = Aws.Util.option_bind (Aws.Xml.member "PublicIp" xml) String.parse
    ; certificate_arn =
        Aws.Util.option_bind (Aws.Xml.member "CertificateArn" xml) String.parse
    ; type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) GatewayType.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; device_name = Aws.Util.option_bind (Aws.Xml.member "DeviceName" xml) String.parse
    ; ip_address = Aws.Util.option_bind (Aws.Xml.member "IpAddress" xml) String.parse
    ; bgp_asn_extended =
        Aws.Util.option_bind (Aws.Xml.member "BgpAsnExtended" xml) Long.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.bgp_asn_extended (fun f ->
             Aws.Query.Pair ("BgpAsnExtended", Long.to_query f))
       ; Aws.Util.option_map v.ip_address (fun f ->
             Aws.Query.Pair ("IpAddress", String.to_query f))
       ; Aws.Util.option_map v.device_name (fun f ->
             Aws.Query.Pair ("DeviceName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some (Aws.Query.Pair ("Type", GatewayType.to_query v.type_))
       ; Aws.Util.option_map v.certificate_arn (fun f ->
             Aws.Query.Pair ("CertificateArn", String.to_query f))
       ; Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ; Aws.Util.option_map v.bgp_asn (fun f ->
             Aws.Query.Pair ("BgpAsn", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.bgp_asn_extended (fun f ->
             "BgpAsnExtended", Long.to_json f)
       ; Aws.Util.option_map v.ip_address (fun f -> "IpAddress", String.to_json f)
       ; Aws.Util.option_map v.device_name (fun f -> "DeviceName", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("Type", GatewayType.to_json v.type_)
       ; Aws.Util.option_map v.certificate_arn (fun f ->
             "CertificateArn", String.to_json f)
       ; Aws.Util.option_map v.public_ip (fun f -> "PublicIp", String.to_json f)
       ; Aws.Util.option_map v.bgp_asn (fun f -> "BgpAsn", Integer.to_json f)
       ])

let of_json j =
  { bgp_asn = Aws.Util.option_map (Aws.Json.lookup j "BgpAsn") Integer.of_json
  ; public_ip = Aws.Util.option_map (Aws.Json.lookup j "PublicIp") String.of_json
  ; certificate_arn =
      Aws.Util.option_map (Aws.Json.lookup j "CertificateArn") String.of_json
  ; type_ = GatewayType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; device_name = Aws.Util.option_map (Aws.Json.lookup j "DeviceName") String.of_json
  ; ip_address = Aws.Util.option_map (Aws.Json.lookup j "IpAddress") String.of_json
  ; bgp_asn_extended =
      Aws.Util.option_map (Aws.Json.lookup j "BgpAsnExtended") Long.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
