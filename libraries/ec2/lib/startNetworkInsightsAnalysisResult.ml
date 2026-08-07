type t = { network_insights_analysis : NetworkInsightsAnalysis.t option }

let make ?network_insights_analysis () = { network_insights_analysis }

let parse xml =
  Some
    { network_insights_analysis =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAnalysis" xml)
          NetworkInsightsAnalysis.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_analysis (fun f ->
             Aws.Query.Pair ("NetworkInsightsAnalysis", NetworkInsightsAnalysis.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_insights_analysis (fun f ->
             "networkInsightsAnalysis", NetworkInsightsAnalysis.to_json f)
       ])

let of_json j =
  { network_insights_analysis =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInsightsAnalysis")
        NetworkInsightsAnalysis.of_json
  }
