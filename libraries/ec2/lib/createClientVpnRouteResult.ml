type t = { status : ClientVpnRouteStatus.t option }

let make ?status () = { status }

let parse xml =
  Some
    { status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) ClientVpnRouteStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientVpnRouteStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "status", ClientVpnRouteStatus.to_json f)
       ])

let of_json j =
  { status = Aws.Util.option_map (Aws.Json.lookup j "status") ClientVpnRouteStatus.of_json
  }
