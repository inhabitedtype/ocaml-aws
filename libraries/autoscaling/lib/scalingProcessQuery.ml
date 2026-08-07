open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; scaling_processes : ProcessNames.t
  }

let make ~auto_scaling_group_name ?(scaling_processes = []) () =
  { auto_scaling_group_name; scaling_processes }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; scaling_processes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ScalingProcesses" xml)
             ProcessNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ScalingProcesses.member", ProcessNames.to_query v.scaling_processes))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ScalingProcesses", ProcessNames.to_json v.scaling_processes)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; scaling_processes =
      ProcessNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScalingProcesses"))
  }
