open Aws.BaseTypes

type t =
  { load_balancer_port : Integer.t option
  ; instance_port : Integer.t option
  }

let make ?load_balancer_port ?instance_port () = { load_balancer_port; instance_port }

let parse xml =
  Some
    { load_balancer_port =
        Aws.Util.option_bind (Aws.Xml.member "loadBalancerPort" xml) Integer.parse
    ; instance_port =
        Aws.Util.option_bind (Aws.Xml.member "instancePort" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_port (fun f ->
             Aws.Query.Pair ("InstancePort", Integer.to_query f))
       ; Aws.Util.option_map v.load_balancer_port (fun f ->
             Aws.Query.Pair ("LoadBalancerPort", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_port (fun f -> "instancePort", Integer.to_json f)
       ; Aws.Util.option_map v.load_balancer_port (fun f ->
             "loadBalancerPort", Integer.to_json f)
       ])

let of_json j =
  { load_balancer_port =
      Aws.Util.option_map (Aws.Json.lookup j "loadBalancerPort") Integer.of_json
  ; instance_port = Aws.Util.option_map (Aws.Json.lookup j "instancePort") Integer.of_json
  }
