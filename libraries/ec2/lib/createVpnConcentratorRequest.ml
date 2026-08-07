open Aws.BaseTypes

type t =
  { type_ : VpnConcentratorType.t
  ; transit_gateway_id : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make ~type_ ?transit_gateway_id ?(tag_specifications = []) ?dry_run () =
  { type_; transit_gateway_id; tag_specifications; dry_run }

let parse xml =
  Some
    { type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) VpnConcentratorType.parse)
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "TransitGatewayId" xml) String.parse
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
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Some (Aws.Query.Pair ("Type", VpnConcentratorType.to_query v.type_))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "TransitGatewayId", String.to_json f)
       ; Some ("Type", VpnConcentratorType.to_json v.type_)
       ])

let of_json j =
  { type_ =
      VpnConcentratorType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "TransitGatewayId") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
