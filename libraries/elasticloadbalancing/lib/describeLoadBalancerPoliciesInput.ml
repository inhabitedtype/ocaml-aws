open Aws.BaseTypes

type t =
  { load_balancer_name : String.t option
  ; policy_names : PolicyNames.t
  }

let make ?load_balancer_name ?(policy_names = []) () =
  { load_balancer_name; policy_names }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse
    ; policy_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
       ; Aws.Util.option_map v.load_balancer_name (fun f ->
             Aws.Query.Pair ("LoadBalancerName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
       ; Aws.Util.option_map v.load_balancer_name (fun f ->
             "LoadBalancerName", String.to_json f)
       ])

let of_json j =
  { load_balancer_name =
      Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerName") String.of_json
  ; policy_names =
      PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
  }
