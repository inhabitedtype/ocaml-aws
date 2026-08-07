open Aws.BaseTypes

type t =
  { trail_name : String.t option
  ; event_data_store : String.t option
  ; max_event_size : MaxEventSize.t option
  ; context_key_selectors : ContextKeySelectors.t
  ; aggregation_configurations : AggregationConfigurations.t
  }

let make
    ?trail_name
    ?event_data_store
    ?max_event_size
    ?(context_key_selectors = [])
    ?(aggregation_configurations = [])
    () =
  { trail_name
  ; event_data_store
  ; max_event_size
  ; context_key_selectors
  ; aggregation_configurations
  }

let parse xml =
  Some
    { trail_name = Aws.Util.option_bind (Aws.Xml.member "TrailName" xml) String.parse
    ; event_data_store =
        Aws.Util.option_bind (Aws.Xml.member "EventDataStore" xml) String.parse
    ; max_event_size =
        Aws.Util.option_bind (Aws.Xml.member "MaxEventSize" xml) MaxEventSize.parse
    ; context_key_selectors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ContextKeySelectors" xml)
             ContextKeySelectors.parse)
    ; aggregation_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AggregationConfigurations" xml)
             AggregationConfigurations.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AggregationConfigurations.member"
              , AggregationConfigurations.to_query v.aggregation_configurations ))
       ; Some
           (Aws.Query.Pair
              ( "ContextKeySelectors.member"
              , ContextKeySelectors.to_query v.context_key_selectors ))
       ; Aws.Util.option_map v.max_event_size (fun f ->
             Aws.Query.Pair ("MaxEventSize", MaxEventSize.to_query f))
       ; Aws.Util.option_map v.event_data_store (fun f ->
             Aws.Query.Pair ("EventDataStore", String.to_query f))
       ; Aws.Util.option_map v.trail_name (fun f ->
             Aws.Query.Pair ("TrailName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AggregationConfigurations"
           , AggregationConfigurations.to_json v.aggregation_configurations )
       ; Some ("ContextKeySelectors", ContextKeySelectors.to_json v.context_key_selectors)
       ; Aws.Util.option_map v.max_event_size (fun f ->
             "MaxEventSize", MaxEventSize.to_json f)
       ; Aws.Util.option_map v.event_data_store (fun f ->
             "EventDataStore", String.to_json f)
       ; Aws.Util.option_map v.trail_name (fun f -> "TrailName", String.to_json f)
       ])

let of_json j =
  { trail_name = Aws.Util.option_map (Aws.Json.lookup j "TrailName") String.of_json
  ; event_data_store =
      Aws.Util.option_map (Aws.Json.lookup j "EventDataStore") String.of_json
  ; max_event_size =
      Aws.Util.option_map (Aws.Json.lookup j "MaxEventSize") MaxEventSize.of_json
  ; context_key_selectors =
      ContextKeySelectors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ContextKeySelectors"))
  ; aggregation_configurations =
      AggregationConfigurations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AggregationConfigurations"))
  }
