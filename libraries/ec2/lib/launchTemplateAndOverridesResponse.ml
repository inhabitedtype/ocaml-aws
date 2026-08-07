type t =
  { launch_template_specification : FleetLaunchTemplateSpecification.t option
  ; overrides : FleetLaunchTemplateOverrides.t option
  }

let make ?launch_template_specification ?overrides () =
  { launch_template_specification; overrides }

let parse xml =
  Some
    { launch_template_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "launchTemplateSpecification" xml)
          FleetLaunchTemplateSpecification.parse
    ; overrides =
        Aws.Util.option_bind
          (Aws.Xml.member "overrides" xml)
          FleetLaunchTemplateOverrides.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.overrides (fun f ->
             Aws.Query.Pair ("Overrides", FleetLaunchTemplateOverrides.to_query f))
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             Aws.Query.Pair
               ("LaunchTemplateSpecification", FleetLaunchTemplateSpecification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.overrides (fun f ->
             "overrides", FleetLaunchTemplateOverrides.to_json f)
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             "launchTemplateSpecification", FleetLaunchTemplateSpecification.to_json f)
       ])

let of_json j =
  { launch_template_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchTemplateSpecification")
        FleetLaunchTemplateSpecification.of_json
  ; overrides =
      Aws.Util.option_map
        (Aws.Json.lookup j "overrides")
        FleetLaunchTemplateOverrides.of_json
  }
