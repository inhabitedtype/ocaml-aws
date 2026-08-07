type t = { launch_template : LaunchTemplate.t option }

let make ?launch_template () = { launch_template }

let parse xml =
  Some
    { launch_template =
        Aws.Util.option_bind (Aws.Xml.member "launchTemplate" xml) LaunchTemplate.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair ("LaunchTemplate", LaunchTemplate.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.launch_template (fun f ->
             "launchTemplate", LaunchTemplate.to_json f)
       ])

let of_json j =
  { launch_template =
      Aws.Util.option_map (Aws.Json.lookup j "launchTemplate") LaunchTemplate.of_json
  }
