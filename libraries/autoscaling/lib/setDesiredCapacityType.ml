open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; desired_capacity : Integer.t
  ; honor_cooldown : Boolean.t option
  }

let make ~auto_scaling_group_name ~desired_capacity ?honor_cooldown () =
  { auto_scaling_group_name; desired_capacity; honor_cooldown }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; desired_capacity =
        Aws.Xml.required
          "DesiredCapacity"
          (Aws.Util.option_bind (Aws.Xml.member "DesiredCapacity" xml) Integer.parse)
    ; honor_cooldown =
        Aws.Util.option_bind (Aws.Xml.member "HonorCooldown" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.honor_cooldown (fun f ->
             Aws.Query.Pair ("HonorCooldown", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("DesiredCapacity", Integer.to_query v.desired_capacity))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.honor_cooldown (fun f ->
             "HonorCooldown", Boolean.to_json f)
       ; Some ("DesiredCapacity", Integer.to_json v.desired_capacity)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; desired_capacity =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DesiredCapacity"))
  ; honor_cooldown =
      Aws.Util.option_map (Aws.Json.lookup j "HonorCooldown") Boolean.of_json
  }
