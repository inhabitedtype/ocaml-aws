open Aws.BaseTypes

type t =
  { transit_gateway_route_table_id : String.t
  ; peering_attachment_id : String.t
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make
    ~transit_gateway_route_table_id
    ~peering_attachment_id
    ?(tag_specifications = [])
    ?dry_run
    () =
  { transit_gateway_route_table_id; peering_attachment_id; tag_specifications; dry_run }

let parse xml =
  Some
    { transit_gateway_route_table_id =
        Aws.Xml.required
          "TransitGatewayRouteTableId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayRouteTableId" xml)
             String.parse)
    ; peering_attachment_id =
        Aws.Xml.required
          "PeeringAttachmentId"
          (Aws.Util.option_bind (Aws.Xml.member "PeeringAttachmentId" xml) String.parse)
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
       ; Some
           (Aws.Query.Pair ("PeeringAttachmentId", String.to_query v.peering_attachment_id))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayRouteTableId"
              , String.to_query v.transit_gateway_route_table_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("PeeringAttachmentId", String.to_json v.peering_attachment_id)
       ; Some
           ("TransitGatewayRouteTableId", String.to_json v.transit_gateway_route_table_id)
       ])

let of_json j =
  { transit_gateway_route_table_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayRouteTableId"))
  ; peering_attachment_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PeeringAttachmentId"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
