open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; load_balancer_attributes : LoadBalancerAttributes.t
  }

let make ~load_balancer_name ~load_balancer_attributes () =
  { load_balancer_name; load_balancer_attributes }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; load_balancer_attributes =
        Aws.Xml.required
          "LoadBalancerAttributes"
          (Aws.Util.option_bind
             (Aws.Xml.member "LoadBalancerAttributes" xml)
             LoadBalancerAttributes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "LoadBalancerAttributes"
              , LoadBalancerAttributes.to_query v.load_balancer_attributes ))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "LoadBalancerAttributes"
           , LoadBalancerAttributes.to_json v.load_balancer_attributes )
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; load_balancer_attributes =
      LoadBalancerAttributes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerAttributes"))
  }
