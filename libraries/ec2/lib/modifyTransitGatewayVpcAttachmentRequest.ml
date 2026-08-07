open Aws.BaseTypes

type t =
  { transit_gateway_attachment_id : String.t
  ; add_subnet_ids : TransitGatewaySubnetIdList.t
  ; remove_subnet_ids : TransitGatewaySubnetIdList.t
  ; options : ModifyTransitGatewayVpcAttachmentRequestOptions.t option
  ; dry_run : Boolean.t option
  }

let make
    ~transit_gateway_attachment_id
    ?(add_subnet_ids = [])
    ?(remove_subnet_ids = [])
    ?options
    ?dry_run
    () =
  { transit_gateway_attachment_id; add_subnet_ids; remove_subnet_ids; options; dry_run }

let parse xml =
  Some
    { transit_gateway_attachment_id =
        Aws.Xml.required
          "TransitGatewayAttachmentId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayAttachmentId" xml)
             String.parse)
    ; add_subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddSubnetIds" xml)
             TransitGatewaySubnetIdList.parse)
    ; remove_subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveSubnetIds" xml)
             TransitGatewaySubnetIdList.parse)
    ; options =
        Aws.Util.option_bind
          (Aws.Xml.member "Options" xml)
          ModifyTransitGatewayVpcAttachmentRequestOptions.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair
               ("Options", ModifyTransitGatewayVpcAttachmentRequestOptions.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RemoveSubnetIds", TransitGatewaySubnetIdList.to_query v.remove_subnet_ids))
       ; Some
           (Aws.Query.Pair
              ("AddSubnetIds", TransitGatewaySubnetIdList.to_query v.add_subnet_ids))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayAttachmentId"
              , String.to_query v.transit_gateway_attachment_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.options (fun f ->
             "Options", ModifyTransitGatewayVpcAttachmentRequestOptions.to_json f)
       ; Some ("RemoveSubnetIds", TransitGatewaySubnetIdList.to_json v.remove_subnet_ids)
       ; Some ("AddSubnetIds", TransitGatewaySubnetIdList.to_json v.add_subnet_ids)
       ; Some
           ("TransitGatewayAttachmentId", String.to_json v.transit_gateway_attachment_id)
       ])

let of_json j =
  { transit_gateway_attachment_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayAttachmentId"))
  ; add_subnet_ids =
      TransitGatewaySubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddSubnetIds"))
  ; remove_subnet_ids =
      TransitGatewaySubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveSubnetIds"))
  ; options =
      Aws.Util.option_map
        (Aws.Json.lookup j "Options")
        ModifyTransitGatewayVpcAttachmentRequestOptions.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
