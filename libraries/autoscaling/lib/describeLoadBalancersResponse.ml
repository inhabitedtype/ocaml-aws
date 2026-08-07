open Aws.BaseTypes

type t =
  { load_balancers : LoadBalancerStates.t
  ; next_token : String.t option
  }

let make ?(load_balancers = []) ?next_token () = { load_balancers; next_token }

let parse xml =
  Some
    { load_balancers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LoadBalancers" xml)
             LoadBalancerStates.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("LoadBalancers.member", LoadBalancerStates.to_query v.load_balancers))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("LoadBalancers", LoadBalancerStates.to_json v.load_balancers)
       ])

let of_json j =
  { load_balancers =
      LoadBalancerStates.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancers"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
