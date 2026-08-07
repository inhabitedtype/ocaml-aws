open Aws.BaseTypes

type t =
  { route_server_endpoint_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~route_server_endpoint_id ?dry_run () = { route_server_endpoint_id; dry_run }

let parse xml =
  Some
    { route_server_endpoint_id =
        Aws.Xml.required
          "RouteServerEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "RouteServerEndpointId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RouteServerEndpointId", String.to_query v.route_server_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("RouteServerEndpointId", String.to_json v.route_server_endpoint_id)
       ])

let of_json j =
  { route_server_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RouteServerEndpointId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
