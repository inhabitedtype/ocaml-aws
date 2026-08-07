open Aws.BaseTypes

type t =
  { auto_scaling_groups : AutoScalingGroups.t
  ; next_token : String.t option
  }

let make ~auto_scaling_groups ?next_token () = { auto_scaling_groups; next_token }

let parse xml =
  Some
    { auto_scaling_groups =
        Aws.Xml.required
          "AutoScalingGroups"
          (Aws.Util.option_bind
             (Aws.Xml.member "AutoScalingGroups" xml)
             AutoScalingGroups.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AutoScalingGroups.member"
              , AutoScalingGroups.to_query v.auto_scaling_groups ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("AutoScalingGroups", AutoScalingGroups.to_json v.auto_scaling_groups)
       ])

let of_json j =
  { auto_scaling_groups =
      AutoScalingGroups.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroups"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
