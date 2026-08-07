open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t option
  ; topic_a_r_n : String.t option
  ; notification_type : String.t option
  }

let make ?auto_scaling_group_name ?topic_a_r_n ?notification_type () =
  { auto_scaling_group_name; topic_a_r_n; notification_type }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
    ; topic_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TopicARN" xml) String.parse
    ; notification_type =
        Aws.Util.option_bind (Aws.Xml.member "NotificationType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.notification_type (fun f ->
             Aws.Query.Pair ("NotificationType", String.to_query f))
       ; Aws.Util.option_map v.topic_a_r_n (fun f ->
             Aws.Query.Pair ("TopicARN", String.to_query f))
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.notification_type (fun f ->
             "NotificationType", String.to_json f)
       ; Aws.Util.option_map v.topic_a_r_n (fun f -> "TopicARN", String.to_json f)
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             "AutoScalingGroupName", String.to_json f)
       ])

let of_json j =
  { auto_scaling_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
  ; topic_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TopicARN") String.of_json
  ; notification_type =
      Aws.Util.option_map (Aws.Json.lookup j "NotificationType") String.of_json
  }
