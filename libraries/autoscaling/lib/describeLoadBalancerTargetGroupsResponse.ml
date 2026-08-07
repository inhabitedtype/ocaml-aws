open Aws.BaseTypes

type t =
  { load_balancer_target_groups : LoadBalancerTargetGroupStates.t
  ; next_token : String.t option
  }

let make ?(load_balancer_target_groups = []) ?next_token () =
  { load_balancer_target_groups; next_token }

let parse xml =
  Some
    { load_balancer_target_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LoadBalancerTargetGroups" xml)
             LoadBalancerTargetGroupStates.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LoadBalancerTargetGroups.member"
              , LoadBalancerTargetGroupStates.to_query v.load_balancer_target_groups ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "LoadBalancerTargetGroups"
           , LoadBalancerTargetGroupStates.to_json v.load_balancer_target_groups )
       ])

let of_json j =
  { load_balancer_target_groups =
      LoadBalancerTargetGroupStates.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerTargetGroups"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
