open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; instance_port : Integer.t
  ; policy_names : PolicyNames.t
  }

let make ~load_balancer_name ~instance_port ~policy_names () =
  { load_balancer_name; instance_port; policy_names }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; instance_port =
        Aws.Xml.required
          "InstancePort"
          (Aws.Util.option_bind (Aws.Xml.member "InstancePort" xml) Integer.parse)
    ; policy_names =
        Aws.Xml.required
          "PolicyNames"
          (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
       ; Some (Aws.Query.Pair ("InstancePort", Integer.to_query v.instance_port))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
       ; Some ("InstancePort", Integer.to_json v.instance_port)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; instance_port =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstancePort"))
  ; policy_names =
      PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
  }
