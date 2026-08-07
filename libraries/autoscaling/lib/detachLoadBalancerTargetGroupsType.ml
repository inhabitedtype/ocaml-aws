open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; target_group_a_r_ns : TargetGroupARNs.t
  }

let make ~auto_scaling_group_name ~target_group_a_r_ns () =
  { auto_scaling_group_name; target_group_a_r_ns }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; target_group_a_r_ns =
        Aws.Xml.required
          "TargetGroupARNs"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetGroupARNs" xml)
             TargetGroupARNs.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TargetGroupARNs.member", TargetGroupARNs.to_query v.target_group_a_r_ns))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TargetGroupARNs", TargetGroupARNs.to_json v.target_group_a_r_ns)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; target_group_a_r_ns =
      TargetGroupARNs.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetGroupARNs"))
  }
