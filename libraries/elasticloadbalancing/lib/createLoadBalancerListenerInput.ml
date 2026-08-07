open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; listeners : Listeners.t
  }

let make ~load_balancer_name ~listeners () = { load_balancer_name; listeners }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; listeners =
        Aws.Xml.required
          "Listeners"
          (Aws.Util.option_bind (Aws.Xml.member "Listeners" xml) Listeners.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Listeners.member", Listeners.to_query v.listeners))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Listeners", Listeners.to_json v.listeners)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; listeners = Listeners.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Listeners"))
  }
