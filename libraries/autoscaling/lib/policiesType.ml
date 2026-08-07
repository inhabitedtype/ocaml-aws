open Aws.BaseTypes

type t =
  { scaling_policies : ScalingPolicies.t
  ; next_token : String.t option
  }

let make ?(scaling_policies = []) ?next_token () = { scaling_policies; next_token }

let parse xml =
  Some
    { scaling_policies =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ScalingPolicies" xml)
             ScalingPolicies.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ScalingPolicies.member", ScalingPolicies.to_query v.scaling_policies))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("ScalingPolicies", ScalingPolicies.to_json v.scaling_policies)
       ])

let of_json j =
  { scaling_policies =
      ScalingPolicies.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ScalingPolicies"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
