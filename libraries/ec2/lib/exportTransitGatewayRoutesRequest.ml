open Aws.BaseTypes

type t =
  { transit_gateway_route_table_id : String.t
  ; filters : FilterList.t
  ; s3_bucket : String.t
  ; dry_run : Boolean.t option
  }

let make ~transit_gateway_route_table_id ?(filters = []) ~s3_bucket ?dry_run () =
  { transit_gateway_route_table_id; filters; s3_bucket; dry_run }

let parse xml =
  Some
    { transit_gateway_route_table_id =
        Aws.Xml.required
          "TransitGatewayRouteTableId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayRouteTableId" xml)
             String.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; s3_bucket =
        Aws.Xml.required
          "S3Bucket"
          (Aws.Util.option_bind (Aws.Xml.member "S3Bucket" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("S3Bucket", String.to_query v.s3_bucket))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayRouteTableId"
              , String.to_query v.transit_gateway_route_table_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("S3Bucket", String.to_json v.s3_bucket)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Some
           ("TransitGatewayRouteTableId", String.to_json v.transit_gateway_route_table_id)
       ])

let of_json j =
  { transit_gateway_route_table_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayRouteTableId"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; s3_bucket = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3Bucket"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
