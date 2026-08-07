open Aws.BaseTypes

type t =
  { route_server_id : String.t option
  ; route_table_id : String.t option
  ; state : RouteServerPropagationState.t option
  }

let make ?route_server_id ?route_table_id ?state () =
  { route_server_id; route_table_id; state }

let parse xml =
  Some
    { route_server_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerId" xml) String.parse
    ; route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "routeTableId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          RouteServerPropagationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", RouteServerPropagationState.to_query f))
       ; Aws.Util.option_map v.route_table_id (fun f ->
             Aws.Query.Pair ("RouteTableId", String.to_query f))
       ; Aws.Util.option_map v.route_server_id (fun f ->
             Aws.Query.Pair ("RouteServerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", RouteServerPropagationState.to_json f)
       ; Aws.Util.option_map v.route_table_id (fun f -> "routeTableId", String.to_json f)
       ; Aws.Util.option_map v.route_server_id (fun f ->
             "routeServerId", String.to_json f)
       ])

let of_json j =
  { route_server_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerId") String.of_json
  ; route_table_id = Aws.Util.option_map (Aws.Json.lookup j "routeTableId") String.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") RouteServerPropagationState.of_json
  }
