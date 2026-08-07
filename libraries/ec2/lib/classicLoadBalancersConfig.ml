type t = { classic_load_balancers : ClassicLoadBalancers.t }

let make ?(classic_load_balancers = []) () = { classic_load_balancers }

let parse xml =
  Some
    { classic_load_balancers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "classicLoadBalancers" xml)
             ClassicLoadBalancers.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ClassicLoadBalancers"
              , ClassicLoadBalancers.to_query v.classic_load_balancers ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("classicLoadBalancers", ClassicLoadBalancers.to_json v.classic_load_balancers)
       ])

let of_json j =
  { classic_load_balancers =
      ClassicLoadBalancers.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "classicLoadBalancers"))
  }
