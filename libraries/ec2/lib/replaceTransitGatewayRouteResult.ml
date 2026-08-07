type t = { route : TransitGatewayRoute.t option }

let make ?route () = { route }

let parse xml =
  Some
    { route = Aws.Util.option_bind (Aws.Xml.member "route" xml) TransitGatewayRoute.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route (fun f ->
             Aws.Query.Pair ("Route", TransitGatewayRoute.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route (fun f -> "route", TransitGatewayRoute.to_json f) ])

let of_json j =
  { route = Aws.Util.option_map (Aws.Json.lookup j "route") TransitGatewayRoute.of_json }
