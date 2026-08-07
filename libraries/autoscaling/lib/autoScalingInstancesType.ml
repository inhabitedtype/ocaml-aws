open Aws.BaseTypes

type t =
  { auto_scaling_instances : AutoScalingInstances.t
  ; next_token : String.t option
  }

let make ?(auto_scaling_instances = []) ?next_token () =
  { auto_scaling_instances; next_token }

let parse xml =
  Some
    { auto_scaling_instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AutoScalingInstances" xml)
             AutoScalingInstances.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AutoScalingInstances.member"
              , AutoScalingInstances.to_query v.auto_scaling_instances ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ("AutoScalingInstances", AutoScalingInstances.to_json v.auto_scaling_instances)
       ])

let of_json j =
  { auto_scaling_instances =
      AutoScalingInstances.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingInstances"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
