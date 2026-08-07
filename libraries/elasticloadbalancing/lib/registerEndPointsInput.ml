open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; instances : Instances.t
  }

let make ~load_balancer_name ~instances () = { load_balancer_name; instances }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; instances =
        Aws.Xml.required
          "Instances"
          (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Instances", Instances.to_json v.instances)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; instances = Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
  }
