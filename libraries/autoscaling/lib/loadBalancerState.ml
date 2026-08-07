open Aws.BaseTypes

type t =
  { load_balancer_name : String.t option
  ; state : String.t option
  }

let make ?load_balancer_name ?state () = { load_balancer_name; state }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.load_balancer_name (fun f ->
             Aws.Query.Pair ("LoadBalancerName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f -> "State", String.to_json f)
       ; Aws.Util.option_map v.load_balancer_name (fun f ->
             "LoadBalancerName", String.to_json f)
       ])

let of_json j =
  { load_balancer_name =
      Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerName") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "State") String.of_json
  }
