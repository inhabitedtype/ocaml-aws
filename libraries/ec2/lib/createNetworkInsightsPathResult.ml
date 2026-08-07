type t = { network_insights_path : NetworkInsightsPath.t option }

let make ?network_insights_path () = { network_insights_path }

let parse xml =
  Some
    { network_insights_path =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsPath" xml)
          NetworkInsightsPath.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_path (fun f ->
             Aws.Query.Pair ("NetworkInsightsPath", NetworkInsightsPath.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_path (fun f ->
             "networkInsightsPath", NetworkInsightsPath.to_json f)
       ])

let of_json j =
  { network_insights_path =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsPath")
        NetworkInsightsPath.of_json
  }
