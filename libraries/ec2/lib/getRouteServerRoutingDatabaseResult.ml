open Aws.BaseTypes

type t =
  { are_routes_persisted : Boolean.t option
  ; routes : RouteServerRouteList.t
  ; next_token : String.t option
  }

let make ?are_routes_persisted ?(routes = []) ?next_token () =
  { are_routes_persisted; routes; next_token }

let parse xml =
  Some
    { are_routes_persisted =
        Aws.Util.option_bind (Aws.Xml.member "areRoutesPersisted" xml) Boolean.parse
    ; routes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeSet" xml)
             RouteServerRouteList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("RouteSet", RouteServerRouteList.to_query v.routes))
       ; Aws.Util.option_map v.are_routes_persisted (fun f ->
             Aws.Query.Pair ("AreRoutesPersisted", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("routeSet", RouteServerRouteList.to_json v.routes)
       ; Aws.Util.option_map v.are_routes_persisted (fun f ->
             "areRoutesPersisted", Boolean.to_json f)
       ])

let of_json j =
  { are_routes_persisted =
      Aws.Util.option_map (Aws.Json.lookup j "areRoutesPersisted") Boolean.of_json
  ; routes =
      RouteServerRouteList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "routeSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
