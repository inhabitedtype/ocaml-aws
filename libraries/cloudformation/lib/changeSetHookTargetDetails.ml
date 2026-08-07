type t =
  { target_type : HookTargetType.t option
  ; resource_target_details : ChangeSetHookResourceTargetDetails.t option
  }

let make ?target_type ?resource_target_details () =
  { target_type; resource_target_details }

let parse xml =
  Some
    { target_type =
        Aws.Util.option_bind (Aws.Xml.member "TargetType" xml) HookTargetType.parse
    ; resource_target_details =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceTargetDetails" xml)
          ChangeSetHookResourceTargetDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_target_details (fun f ->
             Aws.Query.Pair
               ("ResourceTargetDetails", ChangeSetHookResourceTargetDetails.to_query f))
       ; Aws.Util.option_map v.target_type (fun f ->
             Aws.Query.Pair ("TargetType", HookTargetType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_target_details (fun f ->
             "ResourceTargetDetails", ChangeSetHookResourceTargetDetails.to_json f)
       ; Aws.Util.option_map v.target_type (fun f ->
             "TargetType", HookTargetType.to_json f)
       ])

let of_json j =
  { target_type =
      Aws.Util.option_map (Aws.Json.lookup j "TargetType") HookTargetType.of_json
  ; resource_target_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResourceTargetDetails")
        ChangeSetHookResourceTargetDetails.of_json
  }
