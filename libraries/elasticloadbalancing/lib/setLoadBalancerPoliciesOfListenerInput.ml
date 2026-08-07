open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; load_balancer_port : Integer.t
  ; policy_names : PolicyNames.t
  }

let make ~load_balancer_name ~load_balancer_port ~policy_names () =
  { load_balancer_name; load_balancer_port; policy_names }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; load_balancer_port =
        Aws.Xml.required
          "LoadBalancerPort"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerPort" xml) Integer.parse)
    ; policy_names =
        Aws.Xml.required
          "PolicyNames"
          (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
       ; Some (Aws.Query.Pair ("LoadBalancerPort", Integer.to_query v.load_balancer_port))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
       ; Some ("LoadBalancerPort", Integer.to_json v.load_balancer_port)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; load_balancer_port =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerPort"))
  ; policy_names =
      PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
  }
