open Aws.BaseTypes

type t =
  { instance_ids : InstanceIds.t
  ; auto_scaling_group_name : String.t
  ; should_decrement_desired_capacity : Boolean.t
  }

let make
    ?(instance_ids = [])
    ~auto_scaling_group_name
    ~should_decrement_desired_capacity
    () =
  { instance_ids; auto_scaling_group_name; should_decrement_desired_capacity }

let parse xml =
  Some
    { instance_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "InstanceIds" xml) InstanceIds.parse)
    ; auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; should_decrement_desired_capacity =
        Aws.Xml.required
          "ShouldDecrementDesiredCapacity"
          (Aws.Util.option_bind
             (Aws.Xml.member "ShouldDecrementDesiredCapacity" xml)
             Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ShouldDecrementDesiredCapacity"
              , Boolean.to_query v.should_decrement_desired_capacity ))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ; Some (Aws.Query.Pair ("InstanceIds.member", InstanceIds.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ShouldDecrementDesiredCapacity"
           , Boolean.to_json v.should_decrement_desired_capacity )
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ; Some ("InstanceIds", InstanceIds.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceIds"))
  ; auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; should_decrement_desired_capacity =
      Boolean.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ShouldDecrementDesiredCapacity"))
  }
