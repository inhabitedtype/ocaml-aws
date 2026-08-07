type t = { status : ClientVpnEndpointStatus.t option }

let make ?status () = { status }

let parse xml =
  Some
    { status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) ClientVpnEndpointStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientVpnEndpointStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             "status", ClientVpnEndpointStatus.to_json f)
       ])

let of_json j =
  { status =
      Aws.Util.option_map (Aws.Json.lookup j "status") ClientVpnEndpointStatus.of_json
  }
