open Aws.BaseTypes

type t =
  { instance_ids : InstanceIds.t
  ; auto_scaling_group_name : String.t
  ; protected_from_scale_in : Boolean.t
  }

let make ~instance_ids ~auto_scaling_group_name ~protected_from_scale_in () =
  { instance_ids; auto_scaling_group_name; protected_from_scale_in }

let parse xml =
  Some
    { instance_ids =
        Aws.Xml.required
          "InstanceIds"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceIds" xml) InstanceIds.parse)
    ; auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; protected_from_scale_in =
        Aws.Xml.required
          "ProtectedFromScaleIn"
          (Aws.Util.option_bind (Aws.Xml.member "ProtectedFromScaleIn" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ProtectedFromScaleIn", Boolean.to_query v.protected_from_scale_in))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ; Some (Aws.Query.Pair ("InstanceIds.member", InstanceIds.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ProtectedFromScaleIn", Boolean.to_json v.protected_from_scale_in)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ; Some ("InstanceIds", InstanceIds.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceIds"))
  ; auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; protected_from_scale_in =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ProtectedFromScaleIn"))
  }
