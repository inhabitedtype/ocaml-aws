open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; metrics : Metrics.t
  }

let make ~auto_scaling_group_name ?(metrics = []) () =
  { auto_scaling_group_name; metrics }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; metrics =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml) Metrics.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Metrics.member", Metrics.to_query v.metrics))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Metrics", Metrics.to_json v.metrics)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; metrics = Metrics.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
  }
