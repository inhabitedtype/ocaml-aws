type t = { d_b_proxy_target_group : DBProxyTargetGroup.t option }

let make ?d_b_proxy_target_group () = { d_b_proxy_target_group }

let parse xml =
  Some
    { d_b_proxy_target_group =
        Aws.Util.option_bind
          (Aws.Xml.member "DBProxyTargetGroup" xml)
          DBProxyTargetGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_proxy_target_group (fun f ->
             Aws.Query.Pair ("DBProxyTargetGroup", DBProxyTargetGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_proxy_target_group (fun f ->
             "DBProxyTargetGroup", DBProxyTargetGroup.to_json f)
       ])

let of_json j =
  { d_b_proxy_target_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBProxyTargetGroup")
        DBProxyTargetGroup.of_json
  }
