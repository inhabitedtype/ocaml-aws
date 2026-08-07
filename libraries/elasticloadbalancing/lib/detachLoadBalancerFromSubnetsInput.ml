open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; subnets : Subnets.t
  }

let make ~load_balancer_name ~subnets () = { load_balancer_name; subnets }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; subnets =
        Aws.Xml.required
          "Subnets"
          (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) Subnets.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Subnets.member", Subnets.to_query v.subnets))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Subnets", Subnets.to_json v.subnets)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
  }
