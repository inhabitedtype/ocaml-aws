open Aws.BaseTypes

type t =
  { transit_gateway_id : String.t
  ; options : CreateTransitGatewayMulticastDomainRequestOptions.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make ~transit_gateway_id ?options ?(tag_specifications = []) ?dry_run () =
  { transit_gateway_id; options; tag_specifications; dry_run }

let parse xml =
  Some
    { transit_gateway_id =
        Aws.Xml.required
          "TransitGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "TransitGatewayId" xml) String.parse)
    ; options =
        Aws.Util.option_bind
          (Aws.Xml.member "Options" xml)
          CreateTransitGatewayMulticastDomainRequestOptions.parse
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
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair
               ("Options", CreateTransitGatewayMulticastDomainRequestOptions.to_query f))
       ; Some (Aws.Query.Pair ("TransitGatewayId", String.to_query v.transit_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.options (fun f ->
             "Options", CreateTransitGatewayMulticastDomainRequestOptions.to_json f)
       ; Some ("TransitGatewayId", String.to_json v.transit_gateway_id)
       ])

let of_json j =
  { transit_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayId"))
  ; options =
      Aws.Util.option_map
        (Aws.Json.lookup j "Options")
        CreateTransitGatewayMulticastDomainRequestOptions.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
