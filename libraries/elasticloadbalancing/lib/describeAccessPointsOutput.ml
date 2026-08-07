open Aws.BaseTypes

type t =
  { load_balancer_descriptions : LoadBalancerDescriptions.t
  ; next_marker : String.t option
  }

let make ?(load_balancer_descriptions = []) ?next_marker () =
  { load_balancer_descriptions; next_marker }

let parse xml =
  Some
    { load_balancer_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LoadBalancerDescriptions" xml)
             LoadBalancerDescriptions.parse)
    ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_marker (fun f ->
             Aws.Query.Pair ("NextMarker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LoadBalancerDescriptions.member"
              , LoadBalancerDescriptions.to_query v.load_balancer_descriptions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
       ; Some
           ( "LoadBalancerDescriptions"
           , LoadBalancerDescriptions.to_json v.load_balancer_descriptions )
       ])

let of_json j =
  { load_balancer_descriptions =
      LoadBalancerDescriptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerDescriptions"))
  ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
  }
