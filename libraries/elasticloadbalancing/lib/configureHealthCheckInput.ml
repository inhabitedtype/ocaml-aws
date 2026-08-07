open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; health_check : HealthCheck.t
  }

let make ~load_balancer_name ~health_check () = { load_balancer_name; health_check }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; health_check =
        Aws.Xml.required
          "HealthCheck"
          (Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("HealthCheck", HealthCheck.to_json v.health_check)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; health_check =
      HealthCheck.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheck"))
  }
