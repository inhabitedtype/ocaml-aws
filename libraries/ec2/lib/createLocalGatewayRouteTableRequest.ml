open Aws.BaseTypes

type t =
  { local_gateway_id : String.t
  ; mode : LocalGatewayRouteTableMode.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make ~local_gateway_id ?mode ?(tag_specifications = []) ?dry_run () =
  { local_gateway_id; mode; tag_specifications; dry_run }

let parse xml =
  Some
    { local_gateway_id =
        Aws.Xml.required
          "LocalGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "LocalGatewayId" xml) String.parse)
    ; mode =
        Aws.Util.option_bind (Aws.Xml.member "Mode" xml) LocalGatewayRouteTableMode.parse
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
       ; Aws.Util.option_map v.mode (fun f ->
             Aws.Query.Pair ("Mode", LocalGatewayRouteTableMode.to_query f))
       ; Some (Aws.Query.Pair ("LocalGatewayId", String.to_query v.local_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.mode (fun f ->
             "Mode", LocalGatewayRouteTableMode.to_json f)
       ; Some ("LocalGatewayId", String.to_json v.local_gateway_id)
       ])

let of_json j =
  { local_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LocalGatewayId"))
  ; mode =
      Aws.Util.option_map (Aws.Json.lookup j "Mode") LocalGatewayRouteTableMode.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
