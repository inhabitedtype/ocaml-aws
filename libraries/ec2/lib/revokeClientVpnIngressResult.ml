type t = { status : ClientVpnAuthorizationRuleStatus.t option }

let make ?status () = { status }

let parse xml =
  Some
    { status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          ClientVpnAuthorizationRuleStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientVpnAuthorizationRuleStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             "status", ClientVpnAuthorizationRuleStatus.to_json f)
       ])

let of_json j =
  { status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        ClientVpnAuthorizationRuleStatus.of_json
  }
