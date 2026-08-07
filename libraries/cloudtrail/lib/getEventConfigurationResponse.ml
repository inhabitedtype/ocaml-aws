open Aws.BaseTypes

type t =
  { trail_a_r_n : String.t option
  ; event_data_store_arn : String.t option
  ; max_event_size : MaxEventSize.t option
  ; context_key_selectors : ContextKeySelectors.t
  ; aggregation_configurations : AggregationConfigurations.t
  }

let make
    ?trail_a_r_n
    ?event_data_store_arn
    ?max_event_size
    ?(context_key_selectors = [])
    ?(aggregation_configurations = [])
    () =
  { trail_a_r_n
  ; event_data_store_arn
  ; max_event_size
  ; context_key_selectors
  ; aggregation_configurations
  }

let parse xml =
  Some
    { trail_a_r_n = Aws.Util.option_bind (Aws.Xml.member "TrailARN" xml) String.parse
    ; event_data_store_arn =
        Aws.Util.option_bind (Aws.Xml.member "EventDataStoreArn" xml) String.parse
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
       ; Aws.Util.option_map v.event_data_store_arn (fun f ->
             Aws.Query.Pair ("EventDataStoreArn", String.to_query f))
       ; Aws.Util.option_map v.trail_a_r_n (fun f ->
             Aws.Query.Pair ("TrailARN", String.to_query f))
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
       ; Aws.Util.option_map v.event_data_store_arn (fun f ->
             "EventDataStoreArn", String.to_json f)
       ; Aws.Util.option_map v.trail_a_r_n (fun f -> "TrailARN", String.to_json f)
       ])

let of_json j =
  { trail_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "TrailARN") String.of_json
  ; event_data_store_arn =
      Aws.Util.option_map (Aws.Json.lookup j "EventDataStoreArn") String.of_json
  ; max_event_size =
      Aws.Util.option_map (Aws.Json.lookup j "MaxEventSize") MaxEventSize.of_json
  ; context_key_selectors =
      ContextKeySelectors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ContextKeySelectors"))
  ; aggregation_configurations =
      AggregationConfigurations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AggregationConfigurations"))
  }
