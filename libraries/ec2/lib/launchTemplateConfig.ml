type t =
  { launch_template_specification : FleetLaunchTemplateSpecification.t option
  ; overrides : LaunchTemplateOverridesList.t
  }

let make ?launch_template_specification ?(overrides = []) () =
  { launch_template_specification; overrides }

let parse xml =
  Some
    { launch_template_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "launchTemplateSpecification" xml)
          FleetLaunchTemplateSpecification.parse
    ; overrides =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "overrides" xml)
             LaunchTemplateOverridesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Overrides", LaunchTemplateOverridesList.to_query v.overrides))
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             Aws.Query.Pair
               ("LaunchTemplateSpecification", FleetLaunchTemplateSpecification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("overrides", LaunchTemplateOverridesList.to_json v.overrides)
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             "launchTemplateSpecification", FleetLaunchTemplateSpecification.to_json f)
       ])

let of_json j =
  { launch_template_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchTemplateSpecification")
        FleetLaunchTemplateSpecification.of_json
  ; overrides =
      LaunchTemplateOverridesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "overrides"))
  }
