type t = { route : LocalGatewayRoute.t option }

let make ?route () = { route }

let parse xml =
  Some
    { route = Aws.Util.option_bind (Aws.Xml.member "route" xml) LocalGatewayRoute.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route (fun f ->
             Aws.Query.Pair ("Route", LocalGatewayRoute.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route (fun f -> "route", LocalGatewayRoute.to_json f) ])

let of_json j =
  { route = Aws.Util.option_map (Aws.Json.lookup j "route") LocalGatewayRoute.of_json }
