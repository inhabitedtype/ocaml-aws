type t = { d_b_proxy_targets : TargetList.t }

let make ?(d_b_proxy_targets = []) () = { d_b_proxy_targets }

let parse xml =
  Some
    { d_b_proxy_targets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyTargets" xml) TargetList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DBProxyTargets.member", TargetList.to_query v.d_b_proxy_targets))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DBProxyTargets", TargetList.to_json v.d_b_proxy_targets) ])

let of_json j =
  { d_b_proxy_targets =
      TargetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyTargets"))
  }
