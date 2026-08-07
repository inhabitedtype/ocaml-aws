open Aws.BaseTypes

type t =
  { local_gateway_route_table_vpc_association_id : String.t option
  ; local_gateway_route_table_id : String.t option
  ; local_gateway_route_table_arn : String.t option
  ; local_gateway_id : String.t option
  ; vpc_id : String.t option
  ; owner_id : String.t option
  ; state : String.t option
  ; tags : TagList.t
  }

let make
    ?local_gateway_route_table_vpc_association_id
    ?local_gateway_route_table_id
    ?local_gateway_route_table_arn
    ?local_gateway_id
    ?vpc_id
    ?owner_id
    ?state
    ?(tags = [])
    () =
  { local_gateway_route_table_vpc_association_id
  ; local_gateway_route_table_id
  ; local_gateway_route_table_arn
  ; local_gateway_id
  ; vpc_id
  ; owner_id
  ; state
  ; tags
  }

let parse xml =
  Some
    { local_gateway_route_table_vpc_association_id =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayRouteTableVpcAssociationId" xml)
          String.parse
    ; local_gateway_route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayRouteTableId" xml) String.parse
    ; local_gateway_route_table_arn =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayRouteTableArn" xml) String.parse
    ; local_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_id (fun f ->
             Aws.Query.Pair ("LocalGatewayId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_route_table_arn (fun f ->
             Aws.Query.Pair ("LocalGatewayRouteTableArn", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             Aws.Query.Pair ("LocalGatewayRouteTableId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_route_table_vpc_association_id (fun f ->
             Aws.Query.Pair ("LocalGatewayRouteTableVpcAssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_id (fun f ->
             "localGatewayId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_route_table_arn (fun f ->
             "localGatewayRouteTableArn", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             "localGatewayRouteTableId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_route_table_vpc_association_id (fun f ->
             "localGatewayRouteTableVpcAssociationId", String.to_json f)
       ])

let of_json j =
  { local_gateway_route_table_vpc_association_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayRouteTableVpcAssociationId")
        String.of_json
  ; local_gateway_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayRouteTableId") String.of_json
  ; local_gateway_route_table_arn =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayRouteTableArn") String.of_json
  ; local_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
