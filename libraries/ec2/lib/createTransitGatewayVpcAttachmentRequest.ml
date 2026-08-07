open Aws.BaseTypes

type t =
  { transit_gateway_id : String.t
  ; vpc_id : String.t
  ; subnet_ids : TransitGatewaySubnetIdList.t
  ; options : CreateTransitGatewayVpcAttachmentRequestOptions.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make
    ~transit_gateway_id
    ~vpc_id
    ~subnet_ids
    ?options
    ?(tag_specifications = [])
    ?dry_run
    () =
  { transit_gateway_id; vpc_id; subnet_ids; options; tag_specifications; dry_run }

let parse xml =
  Some
    { transit_gateway_id =
        Aws.Xml.required
          "TransitGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "TransitGatewayId" xml) String.parse)
    ; vpc_id =
        Aws.Xml.required
          "VpcId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse)
    ; subnet_ids =
        Aws.Xml.required
          "SubnetIds"
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetIds" xml)
             TransitGatewaySubnetIdList.parse)
    ; options =
        Aws.Util.option_bind
          (Aws.Xml.member "Options" xml)
          CreateTransitGatewayVpcAttachmentRequestOptions.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecifications" xml)
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
              ("TagSpecifications", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair
               ("Options", CreateTransitGatewayVpcAttachmentRequestOptions.to_query f))
       ; Some
           (Aws.Query.Pair ("SubnetIds", TransitGatewaySubnetIdList.to_query v.subnet_ids))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Some (Aws.Query.Pair ("TransitGatewayId", String.to_query v.transit_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecifications", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.options (fun f ->
             "Options", CreateTransitGatewayVpcAttachmentRequestOptions.to_json f)
       ; Some ("SubnetIds", TransitGatewaySubnetIdList.to_json v.subnet_ids)
       ; Some ("VpcId", String.to_json v.vpc_id)
       ; Some ("TransitGatewayId", String.to_json v.transit_gateway_id)
       ])

let of_json j =
  { transit_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayId"))
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; subnet_ids =
      TransitGatewaySubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
  ; options =
      Aws.Util.option_map
        (Aws.Json.lookup j "Options")
        CreateTransitGatewayVpcAttachmentRequestOptions.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecifications"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
