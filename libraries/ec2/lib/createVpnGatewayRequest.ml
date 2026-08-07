open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; type_ : GatewayType.t
  ; tag_specifications : TagSpecificationList.t
  ; amazon_side_asn : Long.t option
  ; dry_run : Boolean.t option
  }

let make ?availability_zone ~type_ ?(tag_specifications = []) ?amazon_side_asn ?dry_run ()
    =
  { availability_zone; type_; tag_specifications; amazon_side_asn; dry_run }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
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
    ; amazon_side_asn =
        Aws.Util.option_bind (Aws.Xml.member "AmazonSideAsn" xml) Long.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.amazon_side_asn (fun f ->
             Aws.Query.Pair ("AmazonSideAsn", Long.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some (Aws.Query.Pair ("Type", GatewayType.to_query v.type_))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.amazon_side_asn (fun f -> "AmazonSideAsn", Long.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("Type", GatewayType.to_json v.type_)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; type_ = GatewayType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; amazon_side_asn = Aws.Util.option_map (Aws.Json.lookup j "AmazonSideAsn") Long.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
