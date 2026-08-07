type t =
  { launch_template_specification : FleetLaunchTemplateSpecificationRequest.t option
  ; overrides : FleetLaunchTemplateOverridesListRequest.t
  }

let make ?launch_template_specification ?(overrides = []) () =
  { launch_template_specification; overrides }

let parse xml =
  Some
    { launch_template_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchTemplateSpecification" xml)
          FleetLaunchTemplateSpecificationRequest.parse
    ; overrides =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Overrides" xml)
             FleetLaunchTemplateOverridesListRequest.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Overrides", FleetLaunchTemplateOverridesListRequest.to_query v.overrides))
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             Aws.Query.Pair
               ( "LaunchTemplateSpecification"
               , FleetLaunchTemplateSpecificationRequest.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Overrides", FleetLaunchTemplateOverridesListRequest.to_json v.overrides)
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             ( "LaunchTemplateSpecification"
             , FleetLaunchTemplateSpecificationRequest.to_json f ))
       ])

let of_json j =
  { launch_template_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchTemplateSpecification")
        FleetLaunchTemplateSpecificationRequest.of_json
  ; overrides =
      FleetLaunchTemplateOverridesListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Overrides"))
  }
