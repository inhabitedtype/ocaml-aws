open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; policy_name : String.t
  }

let make ~load_balancer_name ~policy_name () = { load_balancer_name; policy_name }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; policy_name =
        Aws.Xml.required
          "PolicyName"
          (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PolicyName", String.to_json v.policy_name)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; policy_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
  }
