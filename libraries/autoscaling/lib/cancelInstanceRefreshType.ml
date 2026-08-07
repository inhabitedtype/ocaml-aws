open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; wait_for_transitioning_instances : Boolean.t option
  }

let make ~auto_scaling_group_name ?wait_for_transitioning_instances () =
  { auto_scaling_group_name; wait_for_transitioning_instances }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; wait_for_transitioning_instances =
        Aws.Util.option_bind
          (Aws.Xml.member "WaitForTransitioningInstances" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.wait_for_transitioning_instances (fun f ->
             Aws.Query.Pair ("WaitForTransitioningInstances", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.wait_for_transitioning_instances (fun f ->
             "WaitForTransitioningInstances", Boolean.to_json f)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; wait_for_transitioning_instances =
      Aws.Util.option_map
        (Aws.Json.lookup j "WaitForTransitioningInstances")
        Boolean.of_json
  }
