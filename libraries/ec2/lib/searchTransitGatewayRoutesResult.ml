open Aws.BaseTypes

type t =
  { routes : TransitGatewayRouteList.t
  ; additional_routes_available : Boolean.t option
  ; next_token : String.t option
  }

let make ?(routes = []) ?additional_routes_available ?next_token () =
  { routes; additional_routes_available; next_token }

let parse xml =
  Some
    { routes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeSet" xml)
             TransitGatewayRouteList.parse)
    ; additional_routes_available =
        Aws.Util.option_bind
          (Aws.Xml.member "additionalRoutesAvailable" xml)
          Boolean.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.additional_routes_available (fun f ->
             Aws.Query.Pair ("AdditionalRoutesAvailable", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("RouteSet", TransitGatewayRouteList.to_query v.routes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Aws.Util.option_map v.additional_routes_available (fun f ->
             "additionalRoutesAvailable", Boolean.to_json f)
       ; Some ("routeSet", TransitGatewayRouteList.to_json v.routes)
       ])

let of_json j =
  { routes =
      TransitGatewayRouteList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "routeSet"))
  ; additional_routes_available =
      Aws.Util.option_map (Aws.Json.lookup j "additionalRoutesAvailable") Boolean.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
