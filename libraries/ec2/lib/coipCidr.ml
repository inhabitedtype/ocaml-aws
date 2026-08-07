open Aws.BaseTypes

type t =
  { cidr : String.t option
  ; coip_pool_id : String.t option
  ; local_gateway_route_table_id : String.t option
  }

let make ?cidr ?coip_pool_id ?local_gateway_route_table_id () =
  { cidr; coip_pool_id; local_gateway_route_table_id }

let parse xml =
  Some
    { cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; coip_pool_id = Aws.Util.option_bind (Aws.Xml.member "coipPoolId" xml) String.parse
    ; local_gateway_route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayRouteTableId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             Aws.Query.Pair ("LocalGatewayRouteTableId", String.to_query f))
       ; Aws.Util.option_map v.coip_pool_id (fun f ->
             Aws.Query.Pair ("CoipPoolId", String.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             "localGatewayRouteTableId", String.to_json f)
       ; Aws.Util.option_map v.coip_pool_id (fun f -> "coipPoolId", String.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ])

let of_json j =
  { cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; coip_pool_id = Aws.Util.option_map (Aws.Json.lookup j "coipPoolId") String.of_json
  ; local_gateway_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayRouteTableId") String.of_json
  }
