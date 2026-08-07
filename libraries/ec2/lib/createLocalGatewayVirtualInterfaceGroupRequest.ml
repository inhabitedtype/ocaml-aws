open Aws.BaseTypes

type t =
  { local_gateway_id : String.t
  ; local_bgp_asn : Integer.t option
  ; local_bgp_asn_extended : Long.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make
    ~local_gateway_id
    ?local_bgp_asn
    ?local_bgp_asn_extended
    ?(tag_specifications = [])
    ?dry_run
    () =
  { local_gateway_id; local_bgp_asn; local_bgp_asn_extended; tag_specifications; dry_run }

let parse xml =
  Some
    { local_gateway_id =
        Aws.Xml.required
          "LocalGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "LocalGatewayId" xml) String.parse)
    ; local_bgp_asn =
        Aws.Util.option_bind (Aws.Xml.member "LocalBgpAsn" xml) Integer.parse
    ; local_bgp_asn_extended =
        Aws.Util.option_bind (Aws.Xml.member "LocalBgpAsnExtended" xml) Long.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.local_bgp_asn_extended (fun f ->
             Aws.Query.Pair ("LocalBgpAsnExtended", Long.to_query f))
       ; Aws.Util.option_map v.local_bgp_asn (fun f ->
             Aws.Query.Pair ("LocalBgpAsn", Integer.to_query f))
       ; Some (Aws.Query.Pair ("LocalGatewayId", String.to_query v.local_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.local_bgp_asn_extended (fun f ->
             "LocalBgpAsnExtended", Long.to_json f)
       ; Aws.Util.option_map v.local_bgp_asn (fun f -> "LocalBgpAsn", Integer.to_json f)
       ; Some ("LocalGatewayId", String.to_json v.local_gateway_id)
       ])

let of_json j =
  { local_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LocalGatewayId"))
  ; local_bgp_asn = Aws.Util.option_map (Aws.Json.lookup j "LocalBgpAsn") Integer.of_json
  ; local_bgp_asn_extended =
      Aws.Util.option_map (Aws.Json.lookup j "LocalBgpAsnExtended") Long.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
