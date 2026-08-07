open Aws.BaseTypes

type t =
  { route_table : RouteTable.t option
  ; client_token : String.t option
  }

let make ?route_table ?client_token () = { route_table; client_token }

let parse xml =
  Some
    { route_table =
        Aws.Util.option_bind (Aws.Xml.member "routeTable" xml) RouteTable.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.route_table (fun f ->
             Aws.Query.Pair ("RouteTable", RouteTable.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.route_table (fun f -> "routeTable", RouteTable.to_json f)
       ])

let of_json j =
  { route_table = Aws.Util.option_map (Aws.Json.lookup j "routeTable") RouteTable.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  }
