open Aws.BaseTypes

type t =
  { routes : LocalGatewayRouteList.t
  ; next_token : String.t option
  }

let make ?(routes = []) ?next_token () = { routes; next_token }

let parse xml =
  Some
    { routes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeSet" xml)
             LocalGatewayRouteList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("RouteSet", LocalGatewayRouteList.to_query v.routes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("routeSet", LocalGatewayRouteList.to_json v.routes)
       ])

let of_json j =
  { routes =
      LocalGatewayRouteList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "routeSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
