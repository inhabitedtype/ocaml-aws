type t =
  { launch_template_and_overrides : LaunchTemplateAndOverridesResponse.t option
  ; lifecycle : InstanceLifecycle.t option
  ; instance_ids : InstanceIdsSet.t
  ; instance_type : InstanceType.t option
  ; platform : PlatformValues.t option
  }

let make
    ?launch_template_and_overrides
    ?lifecycle
    ?(instance_ids = [])
    ?instance_type
    ?platform
    () =
  { launch_template_and_overrides; lifecycle; instance_ids; instance_type; platform }

let parse xml =
  Some
    { launch_template_and_overrides =
        Aws.Util.option_bind
          (Aws.Xml.member "launchTemplateAndOverrides" xml)
          LaunchTemplateAndOverridesResponse.parse
    ; lifecycle =
        Aws.Util.option_bind (Aws.Xml.member "lifecycle" xml) InstanceLifecycle.parse
    ; instance_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "instanceIds" xml) InstanceIdsSet.parse)
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "platform" xml) PlatformValues.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", PlatformValues.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Some (Aws.Query.Pair ("InstanceIds", InstanceIdsSet.to_query v.instance_ids))
       ; Aws.Util.option_map v.lifecycle (fun f ->
             Aws.Query.Pair ("Lifecycle", InstanceLifecycle.to_query f))
       ; Aws.Util.option_map v.launch_template_and_overrides (fun f ->
             Aws.Query.Pair
               ( "LaunchTemplateAndOverrides"
               , LaunchTemplateAndOverridesResponse.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.platform (fun f -> "platform", PlatformValues.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Some ("instanceIds", InstanceIdsSet.to_json v.instance_ids)
       ; Aws.Util.option_map v.lifecycle (fun f ->
             "lifecycle", InstanceLifecycle.to_json f)
       ; Aws.Util.option_map v.launch_template_and_overrides (fun f ->
             "launchTemplateAndOverrides", LaunchTemplateAndOverridesResponse.to_json f)
       ])

let of_json j =
  { launch_template_and_overrides =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchTemplateAndOverrides")
        LaunchTemplateAndOverridesResponse.of_json
  ; lifecycle =
      Aws.Util.option_map (Aws.Json.lookup j "lifecycle") InstanceLifecycle.of_json
  ; instance_ids =
      InstanceIdsSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceIds"))
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "platform") PlatformValues.of_json
  }
