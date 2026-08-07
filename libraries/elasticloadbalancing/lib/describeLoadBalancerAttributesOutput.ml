type t = { load_balancer_attributes : LoadBalancerAttributes.t option }

let make ?load_balancer_attributes () = { load_balancer_attributes }

let parse xml =
  Some
    { load_balancer_attributes =
        Aws.Util.option_bind
          (Aws.Xml.member "LoadBalancerAttributes" xml)
          LoadBalancerAttributes.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.load_balancer_attributes (fun f ->
             Aws.Query.Pair ("LoadBalancerAttributes", LoadBalancerAttributes.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.load_balancer_attributes (fun f ->
             "LoadBalancerAttributes", LoadBalancerAttributes.to_json f)
       ])

let of_json j =
  { load_balancer_attributes =
      Aws.Util.option_map
        (Aws.Json.lookup j "LoadBalancerAttributes")
        LoadBalancerAttributes.of_json
  }
