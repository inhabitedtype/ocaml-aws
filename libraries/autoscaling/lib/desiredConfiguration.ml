type t =
  { launch_template : LaunchTemplateSpecification.t option
  ; mixed_instances_policy : MixedInstancesPolicy.t option
  }

let make ?launch_template ?mixed_instances_policy () =
  { launch_template; mixed_instances_policy }

let parse xml =
  Some
    { launch_template =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchTemplate" xml)
          LaunchTemplateSpecification.parse
    ; mixed_instances_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "MixedInstancesPolicy" xml)
          MixedInstancesPolicy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.mixed_instances_policy (fun f ->
             Aws.Query.Pair ("MixedInstancesPolicy", MixedInstancesPolicy.to_query f))
       ; Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair ("LaunchTemplate", LaunchTemplateSpecification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.mixed_instances_policy (fun f ->
             "MixedInstancesPolicy", MixedInstancesPolicy.to_json f)
       ; Aws.Util.option_map v.launch_template (fun f ->
             "LaunchTemplate", LaunchTemplateSpecification.to_json f)
       ])

let of_json j =
  { launch_template =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchTemplate")
        LaunchTemplateSpecification.of_json
  ; mixed_instances_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "MixedInstancesPolicy")
        MixedInstancesPolicy.of_json
  }
