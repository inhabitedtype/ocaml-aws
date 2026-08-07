type t = { launch_template_data : ResponseLaunchTemplateData.t option }

let make ?launch_template_data () = { launch_template_data }

let parse xml =
  Some
    { launch_template_data =
        Aws.Util.option_bind
          (Aws.Xml.member "launchTemplateData" xml)
          ResponseLaunchTemplateData.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.launch_template_data (fun f ->
             Aws.Query.Pair ("LaunchTemplateData", ResponseLaunchTemplateData.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.launch_template_data (fun f ->
             "launchTemplateData", ResponseLaunchTemplateData.to_json f)
       ])

let of_json j =
  { launch_template_data =
      Aws.Util.option_map
        (Aws.Json.lookup j "launchTemplateData")
        ResponseLaunchTemplateData.of_json
  }
