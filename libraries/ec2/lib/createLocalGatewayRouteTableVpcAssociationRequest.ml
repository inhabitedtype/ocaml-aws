open Aws.BaseTypes

type t =
  { local_gateway_route_table_id : String.t
  ; vpc_id : String.t
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make ~local_gateway_route_table_id ~vpc_id ?(tag_specifications = []) ?dry_run () =
  { local_gateway_route_table_id; vpc_id; tag_specifications; dry_run }

let parse xml =
  Some
    { local_gateway_route_table_id =
        Aws.Xml.required
          "LocalGatewayRouteTableId"
          (Aws.Util.option_bind
             (Aws.Xml.member "LocalGatewayRouteTableId" xml)
             String.parse)
    ; vpc_id =
        Aws.Xml.required
          "VpcId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse)
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
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Some
           (Aws.Query.Pair
              ("LocalGatewayRouteTableId", String.to_query v.local_gateway_route_table_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("VpcId", String.to_json v.vpc_id)
       ; Some ("LocalGatewayRouteTableId", String.to_json v.local_gateway_route_table_id)
       ])

let of_json j =
  { local_gateway_route_table_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LocalGatewayRouteTableId"))
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
