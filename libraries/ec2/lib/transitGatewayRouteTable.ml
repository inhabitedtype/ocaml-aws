open Aws.BaseTypes

type t =
  { transit_gateway_route_table_id : String.t option
  ; transit_gateway_id : String.t option
  ; state : TransitGatewayRouteTableState.t option
  ; default_association_route_table : Boolean.t option
  ; default_propagation_route_table : Boolean.t option
  ; creation_time : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?transit_gateway_route_table_id
    ?transit_gateway_id
    ?state
    ?default_association_route_table
    ?default_propagation_route_table
    ?creation_time
    ?(tags = [])
    () =
  { transit_gateway_route_table_id
  ; transit_gateway_id
  ; state
  ; default_association_route_table
  ; default_propagation_route_table
  ; creation_time
  ; tags
  }

let parse xml =
  Some
    { transit_gateway_route_table_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableId" xml)
          String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayRouteTableState.parse
    ; default_association_route_table =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultAssociationRouteTable" xml)
          Boolean.parse
    ; default_propagation_route_table =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultPropagationRouteTable" xml)
          Boolean.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.default_propagation_route_table (fun f ->
             Aws.Query.Pair ("DefaultPropagationRouteTable", Boolean.to_query f))
       ; Aws.Util.option_map v.default_association_route_table (fun f ->
             Aws.Query.Pair ("DefaultAssociationRouteTable", Boolean.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayRouteTableState.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_route_table_id (fun f ->
             Aws.Query.Pair ("TransitGatewayRouteTableId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.default_propagation_route_table (fun f ->
             "defaultPropagationRouteTable", Boolean.to_json f)
       ; Aws.Util.option_map v.default_association_route_table (fun f ->
             "defaultAssociationRouteTable", Boolean.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayRouteTableState.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_route_table_id (fun f ->
             "transitGatewayRouteTableId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayRouteTableId") String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayRouteTableState.of_json
  ; default_association_route_table =
      Aws.Util.option_map
        (Aws.Json.lookup j "defaultAssociationRouteTable")
        Boolean.of_json
  ; default_propagation_route_table =
      Aws.Util.option_map
        (Aws.Json.lookup j "defaultPropagationRouteTable")
        Boolean.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
