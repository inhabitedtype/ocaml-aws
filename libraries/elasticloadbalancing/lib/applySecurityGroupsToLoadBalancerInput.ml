open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; security_groups : SecurityGroups.t
  }

let make ~load_balancer_name ~security_groups () = { load_balancer_name; security_groups }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; security_groups =
        Aws.Xml.required
          "SecurityGroups"
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroups" xml)
             SecurityGroups.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("SecurityGroups.member", SecurityGroups.to_query v.security_groups))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SecurityGroups", SecurityGroups.to_json v.security_groups)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; security_groups =
      SecurityGroups.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
  }
