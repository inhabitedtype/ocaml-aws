type t =
  { launch_template : LaunchTemplate.t option
  ; instances_distribution : InstancesDistribution.t option
  }

let make ?launch_template ?instances_distribution () =
  { launch_template; instances_distribution }

let parse xml =
  Some
    { launch_template =
        Aws.Util.option_bind (Aws.Xml.member "LaunchTemplate" xml) LaunchTemplate.parse
    ; instances_distribution =
        Aws.Util.option_bind
          (Aws.Xml.member "InstancesDistribution" xml)
          InstancesDistribution.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instances_distribution (fun f ->
             Aws.Query.Pair ("InstancesDistribution", InstancesDistribution.to_query f))
       ; Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair ("LaunchTemplate", LaunchTemplate.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instances_distribution (fun f ->
             "InstancesDistribution", InstancesDistribution.to_json f)
       ; Aws.Util.option_map v.launch_template (fun f ->
             "LaunchTemplate", LaunchTemplate.to_json f)
       ])

let of_json j =
  { launch_template =
      Aws.Util.option_map (Aws.Json.lookup j "LaunchTemplate") LaunchTemplate.of_json
  ; instances_distribution =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstancesDistribution")
        InstancesDistribution.of_json
  }
