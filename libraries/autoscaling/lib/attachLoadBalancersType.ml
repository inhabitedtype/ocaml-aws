open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; load_balancer_names : LoadBalancerNames.t
  }

let make ~auto_scaling_group_name ~load_balancer_names () =
  { auto_scaling_group_name; load_balancer_names }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; load_balancer_names =
        Aws.Xml.required
          "LoadBalancerNames"
          (Aws.Util.option_bind
             (Aws.Xml.member "LoadBalancerNames" xml)
             LoadBalancerNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "LoadBalancerNames.member"
              , LoadBalancerNames.to_query v.load_balancer_names ))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; load_balancer_names =
      LoadBalancerNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
  }
