open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; load_balancer_ports : Ports.t
  }

let make ~load_balancer_name ~load_balancer_ports () =
  { load_balancer_name; load_balancer_ports }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; load_balancer_ports =
        Aws.Xml.required
          "LoadBalancerPorts"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerPorts" xml) Ports.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("LoadBalancerPorts.member", Ports.to_query v.load_balancer_ports))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LoadBalancerPorts", Ports.to_json v.load_balancer_ports)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; load_balancer_ports =
      Ports.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerPorts"))
  }
