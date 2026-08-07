open Aws.BaseTypes

type t =
  { route_server_id : String.t option
  ; vpc_id : String.t option
  ; state : RouteServerAssociationState.t option
  }

let make ?route_server_id ?vpc_id ?state () = { route_server_id; vpc_id; state }

let parse xml =
  Some
    { route_server_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          RouteServerAssociationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", RouteServerAssociationState.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.route_server_id (fun f ->
             Aws.Query.Pair ("RouteServerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", RouteServerAssociationState.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.route_server_id (fun f ->
             "routeServerId", String.to_json f)
       ])

let of_json j =
  { route_server_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") RouteServerAssociationState.of_json
  }
