open Aws.BaseTypes

type t =
  { main : Boolean.t option
  ; route_table_association_id : String.t option
  ; route_table_id : String.t option
  ; subnet_id : String.t option
  ; gateway_id : String.t option
  ; public_ipv4_pool : String.t option
  ; association_state : RouteTableAssociationState.t option
  }

let make
    ?main
    ?route_table_association_id
    ?route_table_id
    ?subnet_id
    ?gateway_id
    ?public_ipv4_pool
    ?association_state
    () =
  { main
  ; route_table_association_id
  ; route_table_id
  ; subnet_id
  ; gateway_id
  ; public_ipv4_pool
  ; association_state
  }

let parse xml =
  Some
    { main = Aws.Util.option_bind (Aws.Xml.member "main" xml) Boolean.parse
    ; route_table_association_id =
        Aws.Util.option_bind (Aws.Xml.member "routeTableAssociationId" xml) String.parse
    ; route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "routeTableId" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; gateway_id = Aws.Util.option_bind (Aws.Xml.member "gatewayId" xml) String.parse
    ; public_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "publicIpv4Pool" xml) String.parse
    ; association_state =
        Aws.Util.option_bind
          (Aws.Xml.member "associationState" xml)
          RouteTableAssociationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.association_state (fun f ->
             Aws.Query.Pair ("AssociationState", RouteTableAssociationState.to_query f))
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             Aws.Query.Pair ("PublicIpv4Pool", String.to_query f))
       ; Aws.Util.option_map v.gateway_id (fun f ->
             Aws.Query.Pair ("GatewayId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.route_table_id (fun f ->
             Aws.Query.Pair ("RouteTableId", String.to_query f))
       ; Aws.Util.option_map v.route_table_association_id (fun f ->
             Aws.Query.Pair ("RouteTableAssociationId", String.to_query f))
       ; Aws.Util.option_map v.main (fun f -> Aws.Query.Pair ("Main", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.association_state (fun f ->
             "associationState", RouteTableAssociationState.to_json f)
       ; Aws.Util.option_map v.public_ipv4_pool (fun f ->
             "publicIpv4Pool", String.to_json f)
       ; Aws.Util.option_map v.gateway_id (fun f -> "gatewayId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.route_table_id (fun f -> "routeTableId", String.to_json f)
       ; Aws.Util.option_map v.route_table_association_id (fun f ->
             "routeTableAssociationId", String.to_json f)
       ; Aws.Util.option_map v.main (fun f -> "main", Boolean.to_json f)
       ])

let of_json j =
  { main = Aws.Util.option_map (Aws.Json.lookup j "main") Boolean.of_json
  ; route_table_association_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeTableAssociationId") String.of_json
  ; route_table_id = Aws.Util.option_map (Aws.Json.lookup j "routeTableId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; gateway_id = Aws.Util.option_map (Aws.Json.lookup j "gatewayId") String.of_json
  ; public_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "publicIpv4Pool") String.of_json
  ; association_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "associationState")
        RouteTableAssociationState.of_json
  }
