open Aws.BaseTypes

type t =
  { routes : ClientVpnRouteSet.t
  ; next_token : String.t option
  }

let make ?(routes = []) ?next_token () = { routes; next_token }

let parse xml =
  Some
    { routes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "routes" xml) ClientVpnRouteSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Routes", ClientVpnRouteSet.to_query v.routes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("routes", ClientVpnRouteSet.to_json v.routes)
       ])

let of_json j =
  { routes =
      ClientVpnRouteSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "routes"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
