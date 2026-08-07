type t =
  { launch_template_version : LaunchTemplateVersion.t option
  ; warning : ValidationWarning.t option
  }

let make ?launch_template_version ?warning () = { launch_template_version; warning }

let parse xml =
  Some
    { launch_template_version =
        Aws.Util.option_bind
          (Aws.Xml.member "launchTemplateVersion" xml)
          LaunchTemplateVersion.parse
    ; warning =
        Aws.Util.option_bind (Aws.Xml.member "warning" xml) ValidationWarning.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.warning (fun f ->
             Aws.Query.Pair ("Warning", ValidationWarning.to_query f))
       ; Aws.Util.option_map v.launch_template_version (fun f ->
             Aws.Query.Pair ("LaunchTemplateVersion", LaunchTemplateVersion.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.warning (fun f -> "warning", ValidationWarning.to_json f)
       ; Aws.Util.option_map v.launch_template_version (fun f ->
             "launchTemplateVersion", LaunchTemplateVersion.to_json f)
       ])

let of_json j =
  { launch_template_version =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchTemplateVersion")
        LaunchTemplateVersion.of_json
  ; warning = Aws.Util.option_map (Aws.Json.lookup j "warning") ValidationWarning.of_json
  }
