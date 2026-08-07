open Aws.BaseTypes

type t =
  { trail_name : String.t option
  ; insight_selectors : InsightSelectors.t
  ; event_data_store : String.t option
  ; insights_destination : String.t option
  }

let make ?trail_name ~insight_selectors ?event_data_store ?insights_destination () =
  { trail_name; insight_selectors; event_data_store; insights_destination }

let parse xml =
  Some
    { trail_name = Aws.Util.option_bind (Aws.Xml.member "TrailName" xml) String.parse
    ; insight_selectors =
        Aws.Xml.required
          "InsightSelectors"
          (Aws.Util.option_bind
             (Aws.Xml.member "InsightSelectors" xml)
             InsightSelectors.parse)
    ; event_data_store =
        Aws.Util.option_bind (Aws.Xml.member "EventDataStore" xml) String.parse
    ; insights_destination =
        Aws.Util.option_bind (Aws.Xml.member "InsightsDestination" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.insights_destination (fun f ->
             Aws.Query.Pair ("InsightsDestination", String.to_query f))
       ; Aws.Util.option_map v.event_data_store (fun f ->
             Aws.Query.Pair ("EventDataStore", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("InsightSelectors.member", InsightSelectors.to_query v.insight_selectors))
       ; Aws.Util.option_map v.trail_name (fun f ->
             Aws.Query.Pair ("TrailName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.insights_destination (fun f ->
             "InsightsDestination", String.to_json f)
       ; Aws.Util.option_map v.event_data_store (fun f ->
             "EventDataStore", String.to_json f)
       ; Some ("InsightSelectors", InsightSelectors.to_json v.insight_selectors)
       ; Aws.Util.option_map v.trail_name (fun f -> "TrailName", String.to_json f)
       ])

let of_json j =
  { trail_name = Aws.Util.option_map (Aws.Json.lookup j "TrailName") String.of_json
  ; insight_selectors =
      InsightSelectors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InsightSelectors"))
  ; event_data_store =
      Aws.Util.option_map (Aws.Json.lookup j "EventDataStore") String.of_json
  ; insights_destination =
      Aws.Util.option_map (Aws.Json.lookup j "InsightsDestination") String.of_json
  }
