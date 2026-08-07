type t = { load_balancer_names : LoadBalancerNamesMax20.t }

let make ~load_balancer_names () = { load_balancer_names }

let parse xml =
  Some
    { load_balancer_names =
        Aws.Xml.required
          "LoadBalancerNames"
          (Aws.Util.option_bind
             (Aws.Xml.member "LoadBalancerNames" xml)
             LoadBalancerNamesMax20.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "LoadBalancerNames.member"
              , LoadBalancerNamesMax20.to_query v.load_balancer_names ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LoadBalancerNames", LoadBalancerNamesMax20.to_json v.load_balancer_names)
       ])

let of_json j =
  { load_balancer_names =
      LoadBalancerNamesMax20.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
  }
