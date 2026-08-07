type t =
  { launch_template : LaunchTemplate.t option
  ; warning : ValidationWarning.t option
  }

let make ?launch_template ?warning () = { launch_template; warning }

let parse xml =
  Some
    { launch_template =
        Aws.Util.option_bind (Aws.Xml.member "launchTemplate" xml) LaunchTemplate.parse
    ; warning =
        Aws.Util.option_bind (Aws.Xml.member "warning" xml) ValidationWarning.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.warning (fun f ->
             Aws.Query.Pair ("Warning", ValidationWarning.to_query f))
       ; Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair ("LaunchTemplate", LaunchTemplate.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.warning (fun f -> "warning", ValidationWarning.to_json f)
       ; Aws.Util.option_map v.launch_template (fun f ->
             "launchTemplate", LaunchTemplate.to_json f)
       ])

let of_json j =
  { launch_template =
      Aws.Util.option_map (Aws.Json.lookup j "launchTemplate") LaunchTemplate.of_json
  ; warning = Aws.Util.option_map (Aws.Json.lookup j "warning") ValidationWarning.of_json
  }
