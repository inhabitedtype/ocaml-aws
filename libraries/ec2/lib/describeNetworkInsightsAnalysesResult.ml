open Aws.BaseTypes

type t =
  { network_insights_analyses : NetworkInsightsAnalysisList.t
  ; next_token : String.t option
  }

let make ?(network_insights_analyses = []) ?next_token () =
  { network_insights_analyses; next_token }

let parse xml =
  Some
    { network_insights_analyses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkInsightsAnalysisSet" xml)
             NetworkInsightsAnalysisList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInsightsAnalysisSet"
              , NetworkInsightsAnalysisList.to_query v.network_insights_analyses ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "networkInsightsAnalysisSet"
           , NetworkInsightsAnalysisList.to_json v.network_insights_analyses )
       ])

let of_json j =
  { network_insights_analyses =
      NetworkInsightsAnalysisList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInsightsAnalysisSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
