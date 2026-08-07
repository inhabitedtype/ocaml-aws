open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; topic_a_r_n : String.t
  }

let make ~auto_scaling_group_name ~topic_a_r_n () =
  { auto_scaling_group_name; topic_a_r_n }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; topic_a_r_n =
        Aws.Xml.required
          "TopicARN"
          (Aws.Util.option_bind (Aws.Xml.member "TopicARN" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TopicARN", String.to_query v.topic_a_r_n))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TopicARN", String.to_json v.topic_a_r_n)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; topic_a_r_n = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TopicARN"))
  }
