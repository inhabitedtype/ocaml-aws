type t =
  { launch_template_specification : LaunchTemplateSpecification.t option
  ; overrides : Overrides.t
  }

let make ?launch_template_specification ?(overrides = []) () =
  { launch_template_specification; overrides }

let parse xml =
  Some
    { launch_template_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchTemplateSpecification" xml)
          LaunchTemplateSpecification.parse
    ; overrides =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Overrides" xml) Overrides.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Overrides.member", Overrides.to_query v.overrides))
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             Aws.Query.Pair
               ("LaunchTemplateSpecification", LaunchTemplateSpecification.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Overrides", Overrides.to_json v.overrides)
       ; Aws.Util.option_map v.launch_template_specification (fun f ->
             "LaunchTemplateSpecification", LaunchTemplateSpecification.to_json f)
       ])

let of_json j =
  { launch_template_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchTemplateSpecification")
        LaunchTemplateSpecification.of_json
  ; overrides = Overrides.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Overrides"))
  }
