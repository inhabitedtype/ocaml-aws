open Aws.BaseTypes

type t =
  { coip_pool_id : String.t option
  ; coip_address_usages : CoipAddressUsageSet.t
  ; local_gateway_route_table_id : String.t option
  ; next_token : String.t option
  }

let make
    ?coip_pool_id
    ?(coip_address_usages = [])
    ?local_gateway_route_table_id
    ?next_token
    () =
  { coip_pool_id; coip_address_usages; local_gateway_route_table_id; next_token }

let parse xml =
  Some
    { coip_pool_id = Aws.Util.option_bind (Aws.Xml.member "coipPoolId" xml) String.parse
    ; coip_address_usages =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "coipAddressUsageSet" xml)
             CoipAddressUsageSet.parse)
    ; local_gateway_route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayRouteTableId" xml) String.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             Aws.Query.Pair ("LocalGatewayRouteTableId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CoipAddressUsageSet", CoipAddressUsageSet.to_query v.coip_address_usages))
       ; Aws.Util.option_map v.coip_pool_id (fun f ->
             Aws.Query.Pair ("CoipPoolId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             "localGatewayRouteTableId", String.to_json f)
       ; Some ("coipAddressUsageSet", CoipAddressUsageSet.to_json v.coip_address_usages)
       ; Aws.Util.option_map v.coip_pool_id (fun f -> "coipPoolId", String.to_json f)
       ])

let of_json j =
  { coip_pool_id = Aws.Util.option_map (Aws.Json.lookup j "coipPoolId") String.of_json
  ; coip_address_usages =
      CoipAddressUsageSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "coipAddressUsageSet"))
  ; local_gateway_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayRouteTableId") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
