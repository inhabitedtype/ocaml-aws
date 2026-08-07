open Aws.BaseTypes

type t =
  { pool_id : String.t option
  ; pool_cidrs : ValueStringList.t
  ; local_gateway_route_table_id : String.t option
  ; tags : TagList.t
  ; pool_arn : String.t option
  }

let make
    ?pool_id
    ?(pool_cidrs = [])
    ?local_gateway_route_table_id
    ?(tags = [])
    ?pool_arn
    () =
  { pool_id; pool_cidrs; local_gateway_route_table_id; tags; pool_arn }

let parse xml =
  Some
    { pool_id = Aws.Util.option_bind (Aws.Xml.member "poolId" xml) String.parse
    ; pool_cidrs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "poolCidrSet" xml) ValueStringList.parse)
    ; local_gateway_route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayRouteTableId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; pool_arn = Aws.Util.option_bind (Aws.Xml.member "poolArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pool_arn (fun f ->
             Aws.Query.Pair ("PoolArn", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             Aws.Query.Pair ("LocalGatewayRouteTableId", String.to_query f))
       ; Some (Aws.Query.Pair ("PoolCidrSet", ValueStringList.to_query v.pool_cidrs))
       ; Aws.Util.option_map v.pool_id (fun f ->
             Aws.Query.Pair ("PoolId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pool_arn (fun f -> "poolArn", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             "localGatewayRouteTableId", String.to_json f)
       ; Some ("poolCidrSet", ValueStringList.to_json v.pool_cidrs)
       ; Aws.Util.option_map v.pool_id (fun f -> "poolId", String.to_json f)
       ])

let of_json j =
  { pool_id = Aws.Util.option_map (Aws.Json.lookup j "poolId") String.of_json
  ; pool_cidrs =
      ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "poolCidrSet"))
  ; local_gateway_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayRouteTableId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; pool_arn = Aws.Util.option_map (Aws.Json.lookup j "poolArn") String.of_json
  }
