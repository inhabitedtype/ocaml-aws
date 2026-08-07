open Aws.BaseTypes

type t =
  { trail_a_r_n : String.t option
  ; insight_selectors : InsightSelectors.t
  ; event_data_store_arn : String.t option
  ; insights_destination : String.t option
  }

let make
    ?trail_a_r_n
    ?(insight_selectors = [])
    ?event_data_store_arn
    ?insights_destination
    () =
  { trail_a_r_n; insight_selectors; event_data_store_arn; insights_destination }

let parse xml =
  Some
    { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
    ; insight_selectors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InsightSelectors" xml)
             InsightSelectors.parse)
    ; event_data_store_arn =
        Aws.Util.option_bind (Aws.Xml.member "EventDataStoreArn" xml) String.parse
    ; insights_destination =
        Aws.Util.option_bind (Aws.Xml.member "InsightsDestination" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.insights_destination (fun f ->
             Aws.Query.Pair ("InsightsDestination", String.to_query f))
       ; Aws.Util.option_map v.event_data_store_arn (fun f ->
             Aws.Query.Pair ("EventDataStoreArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("InsightSelectors.member", InsightSelectors.to_query v.insight_selectors))
       ; Aws.Util.option_map v.trail_a_r_n (fun f ->
             Aws.Query.Pair ("TrailARN", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.insights_destination (fun f ->
             "InsightsDestination", String.to_json f)
       ; Aws.Util.option_map v.event_data_store_arn (fun f ->
             "EventDataStoreArn", String.to_json f)
       ; Some ("InsightSelectors", InsightSelectors.to_json v.insight_selectors)
       ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
       ])

let of_json j =
  { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
  ; insight_selectors =
      InsightSelectors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InsightSelectors"))
  ; event_data_store_arn =
      Aws.Util.option_map (Aws.Json.lookup j "EventDataStoreArn") String.of_json
  ; insights_destination =
      Aws.Util.option_map (Aws.Json.lookup j "InsightsDestination") String.of_json
  }
