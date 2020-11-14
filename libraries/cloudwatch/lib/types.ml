open Aws
open Aws.BaseTypes
open CalendarLib

type calendar = Calendar.t

module Dimension = struct
  type t =
    { name : String.t
    ; value : String.t
    }

  let make ~name ~value () = { name; value }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; value =
          Xml.required "Value" (Util.option_bind (Xml.member "Value" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Value", String.to_query v.value))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("value", String.to_json v.value); Some ("name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; value = String.of_json (Util.of_option_exn (Json.lookup j "value"))
    }
end

module HistoryItemType = struct
  type t =
    | ConfigurationUpdate
    | StateUpdate
    | Action

  let str_to_t =
    [ "Action", Action
    ; "StateUpdate", StateUpdate
    ; "ConfigurationUpdate", ConfigurationUpdate
    ]

  let t_to_str =
    [ Action, "Action"
    ; StateUpdate, "StateUpdate"
    ; ConfigurationUpdate, "ConfigurationUpdate"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ComparisonOperator = struct
  type t =
    | GreaterThanOrEqualToThreshold
    | GreaterThanThreshold
    | LessThanThreshold
    | LessThanOrEqualToThreshold

  let str_to_t =
    [ "LessThanOrEqualToThreshold", LessThanOrEqualToThreshold
    ; "LessThanThreshold", LessThanThreshold
    ; "GreaterThanThreshold", GreaterThanThreshold
    ; "GreaterThanOrEqualToThreshold", GreaterThanOrEqualToThreshold
    ]

  let t_to_str =
    [ LessThanOrEqualToThreshold, "LessThanOrEqualToThreshold"
    ; LessThanThreshold, "LessThanThreshold"
    ; GreaterThanThreshold, "GreaterThanThreshold"
    ; GreaterThanOrEqualToThreshold, "GreaterThanOrEqualToThreshold"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module Dimensions = struct
  type t = Dimension.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Dimension.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Dimension.to_query v

  let to_json v = `List (List.map Dimension.to_json v)

  let of_json j = Json.to_list Dimension.of_json j
end

module ResourceList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module StandardUnit = struct
  type t =
    | Seconds
    | Microseconds
    | Milliseconds
    | Bytes
    | Kilobytes
    | Megabytes
    | Gigabytes
    | Terabytes
    | Bits
    | Kilobits
    | Megabits
    | Gigabits
    | Terabits
    | Percent
    | Count
    | Bytes_Second
    | Kilobytes_Second
    | Megabytes_Second
    | Gigabytes_Second
    | Terabytes_Second
    | Bits_Second
    | Kilobits_Second
    | Megabits_Second
    | Gigabits_Second
    | Terabits_Second
    | Count_Second
    | None

  let str_to_t =
    [ "None", None
    ; "Count/Second", Count_Second
    ; "Terabits/Second", Terabits_Second
    ; "Gigabits/Second", Gigabits_Second
    ; "Megabits/Second", Megabits_Second
    ; "Kilobits/Second", Kilobits_Second
    ; "Bits/Second", Bits_Second
    ; "Terabytes/Second", Terabytes_Second
    ; "Gigabytes/Second", Gigabytes_Second
    ; "Megabytes/Second", Megabytes_Second
    ; "Kilobytes/Second", Kilobytes_Second
    ; "Bytes/Second", Bytes_Second
    ; "Count", Count
    ; "Percent", Percent
    ; "Terabits", Terabits
    ; "Gigabits", Gigabits
    ; "Megabits", Megabits
    ; "Kilobits", Kilobits
    ; "Bits", Bits
    ; "Terabytes", Terabytes
    ; "Gigabytes", Gigabytes
    ; "Megabytes", Megabytes
    ; "Kilobytes", Kilobytes
    ; "Bytes", Bytes
    ; "Milliseconds", Milliseconds
    ; "Microseconds", Microseconds
    ; "Seconds", Seconds
    ]

  let t_to_str =
    [ None, "None"
    ; Count_Second, "Count/Second"
    ; Terabits_Second, "Terabits/Second"
    ; Gigabits_Second, "Gigabits/Second"
    ; Megabits_Second, "Megabits/Second"
    ; Kilobits_Second, "Kilobits/Second"
    ; Bits_Second, "Bits/Second"
    ; Terabytes_Second, "Terabytes/Second"
    ; Gigabytes_Second, "Gigabytes/Second"
    ; Megabytes_Second, "Megabytes/Second"
    ; Kilobytes_Second, "Kilobytes/Second"
    ; Bytes_Second, "Bytes/Second"
    ; Count, "Count"
    ; Percent, "Percent"
    ; Terabits, "Terabits"
    ; Gigabits, "Gigabits"
    ; Megabits, "Megabits"
    ; Kilobits, "Kilobits"
    ; Bits, "Bits"
    ; Terabytes, "Terabytes"
    ; Gigabytes, "Gigabytes"
    ; Megabytes, "Megabytes"
    ; Kilobytes, "Kilobytes"
    ; Bytes, "Bytes"
    ; Milliseconds, "Milliseconds"
    ; Microseconds, "Microseconds"
    ; Seconds, "Seconds"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module StateValue = struct
  type t =
    | OK
    | ALARM
    | INSUFFICIENT_DATA

  let str_to_t = [ "INSUFFICIENT_DATA", INSUFFICIENT_DATA; "ALARM", ALARM; "OK", OK ]

  let t_to_str = [ INSUFFICIENT_DATA, "INSUFFICIENT_DATA"; ALARM, "ALARM"; OK, "OK" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module Statistic = struct
  type t =
    | SampleCount
    | Average
    | Sum
    | Minimum
    | Maximum

  let str_to_t =
    [ "Maximum", Maximum
    ; "Minimum", Minimum
    ; "Sum", Sum
    ; "Average", Average
    ; "SampleCount", SampleCount
    ]

  let t_to_str =
    [ Maximum, "Maximum"
    ; Minimum, "Minimum"
    ; Sum, "Sum"
    ; Average, "Average"
    ; SampleCount, "SampleCount"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module StatisticSet = struct
  type t =
    { sample_count : Double.t
    ; sum : Double.t
    ; minimum : Double.t
    ; maximum : Double.t
    }

  let make ~sample_count ~sum ~minimum ~maximum () =
    { sample_count; sum; minimum; maximum }

  let parse xml =
    Some
      { sample_count =
          Xml.required
            "SampleCount"
            (Util.option_bind (Xml.member "SampleCount" xml) Double.parse)
      ; sum = Xml.required "Sum" (Util.option_bind (Xml.member "Sum" xml) Double.parse)
      ; minimum =
          Xml.required
            "Minimum"
            (Util.option_bind (Xml.member "Minimum" xml) Double.parse)
      ; maximum =
          Xml.required
            "Maximum"
            (Util.option_bind (Xml.member "Maximum" xml) Double.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Maximum", Double.to_query v.maximum))
         ; Some (Query.Pair ("Minimum", Double.to_query v.minimum))
         ; Some (Query.Pair ("Sum", Double.to_query v.sum))
         ; Some (Query.Pair ("SampleCount", Double.to_query v.sample_count))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("maximum", Double.to_json v.maximum)
         ; Some ("minimum", Double.to_json v.minimum)
         ; Some ("sum", Double.to_json v.sum)
         ; Some ("sample_count", Double.to_json v.sample_count)
         ])

  let of_json j =
    { sample_count = Double.of_json (Util.of_option_exn (Json.lookup j "sample_count"))
    ; sum = Double.of_json (Util.of_option_exn (Json.lookup j "sum"))
    ; minimum = Double.of_json (Util.of_option_exn (Json.lookup j "minimum"))
    ; maximum = Double.of_json (Util.of_option_exn (Json.lookup j "maximum"))
    }
end

module AlarmHistoryItem = struct
  type t =
    { alarm_name : String.t option
    ; timestamp : DateTime.t option
    ; history_item_type : HistoryItemType.t option
    ; history_summary : String.t option
    ; history_data : String.t option
    }

  let make ?alarm_name ?timestamp ?history_item_type ?history_summary ?history_data () =
    { alarm_name; timestamp; history_item_type; history_summary; history_data }

  let parse xml =
    Some
      { alarm_name = Util.option_bind (Xml.member "AlarmName" xml) String.parse
      ; timestamp = Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse
      ; history_item_type =
          Util.option_bind (Xml.member "HistoryItemType" xml) HistoryItemType.parse
      ; history_summary = Util.option_bind (Xml.member "HistorySummary" xml) String.parse
      ; history_data = Util.option_bind (Xml.member "HistoryData" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.history_data (fun f ->
               Query.Pair ("HistoryData", String.to_query f))
         ; Util.option_map v.history_summary (fun f ->
               Query.Pair ("HistorySummary", String.to_query f))
         ; Util.option_map v.history_item_type (fun f ->
               Query.Pair ("HistoryItemType", HistoryItemType.to_query f))
         ; Util.option_map v.timestamp (fun f ->
               Query.Pair ("Timestamp", DateTime.to_query f))
         ; Util.option_map v.alarm_name (fun f ->
               Query.Pair ("AlarmName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.history_data (fun f -> "history_data", String.to_json f)
         ; Util.option_map v.history_summary (fun f ->
               "history_summary", String.to_json f)
         ; Util.option_map v.history_item_type (fun f ->
               "history_item_type", HistoryItemType.to_json f)
         ; Util.option_map v.timestamp (fun f -> "timestamp", DateTime.to_json f)
         ; Util.option_map v.alarm_name (fun f -> "alarm_name", String.to_json f)
         ])

  let of_json j =
    { alarm_name = Util.option_map (Json.lookup j "alarm_name") String.of_json
    ; timestamp = Util.option_map (Json.lookup j "timestamp") DateTime.of_json
    ; history_item_type =
        Util.option_map (Json.lookup j "history_item_type") HistoryItemType.of_json
    ; history_summary = Util.option_map (Json.lookup j "history_summary") String.of_json
    ; history_data = Util.option_map (Json.lookup j "history_data") String.of_json
    }
end

module DimensionFilter = struct
  type t =
    { name : String.t
    ; value : String.t option
    }

  let make ~name ?value () = { name; value }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; value = Util.option_bind (Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> Query.Pair ("Value", String.to_query f))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> "value", String.to_json f)
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; value = Util.option_map (Json.lookup j "value") String.of_json
    }
end

module MetricAlarm = struct
  type t =
    { alarm_name : String.t option
    ; alarm_arn : String.t option
    ; alarm_description : String.t option
    ; alarm_configuration_updated_timestamp : DateTime.t option
    ; actions_enabled : Boolean.t option
    ; o_k_actions : ResourceList.t
    ; alarm_actions : ResourceList.t
    ; insufficient_data_actions : ResourceList.t
    ; state_value : StateValue.t option
    ; state_reason : String.t option
    ; state_reason_data : String.t option
    ; state_updated_timestamp : DateTime.t option
    ; metric_name : String.t option
    ; namespace : String.t option
    ; statistic : Statistic.t option
    ; dimensions : Dimensions.t
    ; period : Integer.t option
    ; unit : StandardUnit.t option
    ; evaluation_periods : Integer.t option
    ; threshold : Double.t option
    ; comparison_operator : ComparisonOperator.t option
    }

  let make
      ?alarm_name
      ?alarm_arn
      ?alarm_description
      ?alarm_configuration_updated_timestamp
      ?actions_enabled
      ?(o_k_actions = [])
      ?(alarm_actions = [])
      ?(insufficient_data_actions = [])
      ?state_value
      ?state_reason
      ?state_reason_data
      ?state_updated_timestamp
      ?metric_name
      ?namespace
      ?statistic
      ?(dimensions = [])
      ?period
      ?unit
      ?evaluation_periods
      ?threshold
      ?comparison_operator
      () =
    { alarm_name
    ; alarm_arn
    ; alarm_description
    ; alarm_configuration_updated_timestamp
    ; actions_enabled
    ; o_k_actions
    ; alarm_actions
    ; insufficient_data_actions
    ; state_value
    ; state_reason
    ; state_reason_data
    ; state_updated_timestamp
    ; metric_name
    ; namespace
    ; statistic
    ; dimensions
    ; period
    ; unit
    ; evaluation_periods
    ; threshold
    ; comparison_operator
    }

  let parse xml =
    Some
      { alarm_name = Util.option_bind (Xml.member "AlarmName" xml) String.parse
      ; alarm_arn = Util.option_bind (Xml.member "AlarmArn" xml) String.parse
      ; alarm_description =
          Util.option_bind (Xml.member "AlarmDescription" xml) String.parse
      ; alarm_configuration_updated_timestamp =
          Util.option_bind
            (Xml.member "AlarmConfigurationUpdatedTimestamp" xml)
            DateTime.parse
      ; actions_enabled = Util.option_bind (Xml.member "ActionsEnabled" xml) Boolean.parse
      ; o_k_actions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "OKActions" xml) ResourceList.parse)
      ; alarm_actions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "AlarmActions" xml) ResourceList.parse)
      ; insufficient_data_actions =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InsufficientDataActions" xml)
               ResourceList.parse)
      ; state_value = Util.option_bind (Xml.member "StateValue" xml) StateValue.parse
      ; state_reason = Util.option_bind (Xml.member "StateReason" xml) String.parse
      ; state_reason_data =
          Util.option_bind (Xml.member "StateReasonData" xml) String.parse
      ; state_updated_timestamp =
          Util.option_bind (Xml.member "StateUpdatedTimestamp" xml) DateTime.parse
      ; metric_name = Util.option_bind (Xml.member "MetricName" xml) String.parse
      ; namespace = Util.option_bind (Xml.member "Namespace" xml) String.parse
      ; statistic = Util.option_bind (Xml.member "Statistic" xml) Statistic.parse
      ; dimensions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Dimensions" xml) Dimensions.parse)
      ; period = Util.option_bind (Xml.member "Period" xml) Integer.parse
      ; unit = Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse
      ; evaluation_periods =
          Util.option_bind (Xml.member "EvaluationPeriods" xml) Integer.parse
      ; threshold = Util.option_bind (Xml.member "Threshold" xml) Double.parse
      ; comparison_operator =
          Util.option_bind (Xml.member "ComparisonOperator" xml) ComparisonOperator.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.comparison_operator (fun f ->
               Query.Pair ("ComparisonOperator", ComparisonOperator.to_query f))
         ; Util.option_map v.threshold (fun f ->
               Query.Pair ("Threshold", Double.to_query f))
         ; Util.option_map v.evaluation_periods (fun f ->
               Query.Pair ("EvaluationPeriods", Integer.to_query f))
         ; Util.option_map v.unit (fun f -> Query.Pair ("Unit", StandardUnit.to_query f))
         ; Util.option_map v.period (fun f -> Query.Pair ("Period", Integer.to_query f))
         ; Some (Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Util.option_map v.statistic (fun f ->
               Query.Pair ("Statistic", Statistic.to_query f))
         ; Util.option_map v.namespace (fun f ->
               Query.Pair ("Namespace", String.to_query f))
         ; Util.option_map v.metric_name (fun f ->
               Query.Pair ("MetricName", String.to_query f))
         ; Util.option_map v.state_updated_timestamp (fun f ->
               Query.Pair ("StateUpdatedTimestamp", DateTime.to_query f))
         ; Util.option_map v.state_reason_data (fun f ->
               Query.Pair ("StateReasonData", String.to_query f))
         ; Util.option_map v.state_reason (fun f ->
               Query.Pair ("StateReason", String.to_query f))
         ; Util.option_map v.state_value (fun f ->
               Query.Pair ("StateValue", StateValue.to_query f))
         ; Some
             (Query.Pair
                ( "InsufficientDataActions.member"
                , ResourceList.to_query v.insufficient_data_actions ))
         ; Some
             (Query.Pair ("AlarmActions.member", ResourceList.to_query v.alarm_actions))
         ; Some (Query.Pair ("OKActions.member", ResourceList.to_query v.o_k_actions))
         ; Util.option_map v.actions_enabled (fun f ->
               Query.Pair ("ActionsEnabled", Boolean.to_query f))
         ; Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
               Query.Pair ("AlarmConfigurationUpdatedTimestamp", DateTime.to_query f))
         ; Util.option_map v.alarm_description (fun f ->
               Query.Pair ("AlarmDescription", String.to_query f))
         ; Util.option_map v.alarm_arn (fun f ->
               Query.Pair ("AlarmArn", String.to_query f))
         ; Util.option_map v.alarm_name (fun f ->
               Query.Pair ("AlarmName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.comparison_operator (fun f ->
               "comparison_operator", ComparisonOperator.to_json f)
         ; Util.option_map v.threshold (fun f -> "threshold", Double.to_json f)
         ; Util.option_map v.evaluation_periods (fun f ->
               "evaluation_periods", Integer.to_json f)
         ; Util.option_map v.unit (fun f -> "unit", StandardUnit.to_json f)
         ; Util.option_map v.period (fun f -> "period", Integer.to_json f)
         ; Some ("dimensions", Dimensions.to_json v.dimensions)
         ; Util.option_map v.statistic (fun f -> "statistic", Statistic.to_json f)
         ; Util.option_map v.namespace (fun f -> "namespace", String.to_json f)
         ; Util.option_map v.metric_name (fun f -> "metric_name", String.to_json f)
         ; Util.option_map v.state_updated_timestamp (fun f ->
               "state_updated_timestamp", DateTime.to_json f)
         ; Util.option_map v.state_reason_data (fun f ->
               "state_reason_data", String.to_json f)
         ; Util.option_map v.state_reason (fun f -> "state_reason", String.to_json f)
         ; Util.option_map v.state_value (fun f -> "state_value", StateValue.to_json f)
         ; Some
             ( "insufficient_data_actions"
             , ResourceList.to_json v.insufficient_data_actions )
         ; Some ("alarm_actions", ResourceList.to_json v.alarm_actions)
         ; Some ("o_k_actions", ResourceList.to_json v.o_k_actions)
         ; Util.option_map v.actions_enabled (fun f ->
               "actions_enabled", Boolean.to_json f)
         ; Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
               "alarm_configuration_updated_timestamp", DateTime.to_json f)
         ; Util.option_map v.alarm_description (fun f ->
               "alarm_description", String.to_json f)
         ; Util.option_map v.alarm_arn (fun f -> "alarm_arn", String.to_json f)
         ; Util.option_map v.alarm_name (fun f -> "alarm_name", String.to_json f)
         ])

  let of_json j =
    { alarm_name = Util.option_map (Json.lookup j "alarm_name") String.of_json
    ; alarm_arn = Util.option_map (Json.lookup j "alarm_arn") String.of_json
    ; alarm_description =
        Util.option_map (Json.lookup j "alarm_description") String.of_json
    ; alarm_configuration_updated_timestamp =
        Util.option_map
          (Json.lookup j "alarm_configuration_updated_timestamp")
          DateTime.of_json
    ; actions_enabled = Util.option_map (Json.lookup j "actions_enabled") Boolean.of_json
    ; o_k_actions =
        ResourceList.of_json (Util.of_option_exn (Json.lookup j "o_k_actions"))
    ; alarm_actions =
        ResourceList.of_json (Util.of_option_exn (Json.lookup j "alarm_actions"))
    ; insufficient_data_actions =
        ResourceList.of_json
          (Util.of_option_exn (Json.lookup j "insufficient_data_actions"))
    ; state_value = Util.option_map (Json.lookup j "state_value") StateValue.of_json
    ; state_reason = Util.option_map (Json.lookup j "state_reason") String.of_json
    ; state_reason_data =
        Util.option_map (Json.lookup j "state_reason_data") String.of_json
    ; state_updated_timestamp =
        Util.option_map (Json.lookup j "state_updated_timestamp") DateTime.of_json
    ; metric_name = Util.option_map (Json.lookup j "metric_name") String.of_json
    ; namespace = Util.option_map (Json.lookup j "namespace") String.of_json
    ; statistic = Util.option_map (Json.lookup j "statistic") Statistic.of_json
    ; dimensions = Dimensions.of_json (Util.of_option_exn (Json.lookup j "dimensions"))
    ; period = Util.option_map (Json.lookup j "period") Integer.of_json
    ; unit = Util.option_map (Json.lookup j "unit") StandardUnit.of_json
    ; evaluation_periods =
        Util.option_map (Json.lookup j "evaluation_periods") Integer.of_json
    ; threshold = Util.option_map (Json.lookup j "threshold") Double.of_json
    ; comparison_operator =
        Util.option_map (Json.lookup j "comparison_operator") ComparisonOperator.of_json
    }
end

module Metric = struct
  type t =
    { namespace : String.t option
    ; metric_name : String.t option
    ; dimensions : Dimensions.t
    }

  let make ?namespace ?metric_name ?(dimensions = []) () =
    { namespace; metric_name; dimensions }

  let parse xml =
    Some
      { namespace = Util.option_bind (Xml.member "Namespace" xml) String.parse
      ; metric_name = Util.option_bind (Xml.member "MetricName" xml) String.parse
      ; dimensions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Dimensions" xml) Dimensions.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Util.option_map v.metric_name (fun f ->
               Query.Pair ("MetricName", String.to_query f))
         ; Util.option_map v.namespace (fun f ->
               Query.Pair ("Namespace", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("dimensions", Dimensions.to_json v.dimensions)
         ; Util.option_map v.metric_name (fun f -> "metric_name", String.to_json f)
         ; Util.option_map v.namespace (fun f -> "namespace", String.to_json f)
         ])

  let of_json j =
    { namespace = Util.option_map (Json.lookup j "namespace") String.of_json
    ; metric_name = Util.option_map (Json.lookup j "metric_name") String.of_json
    ; dimensions = Dimensions.of_json (Util.of_option_exn (Json.lookup j "dimensions"))
    }
end

module MetricDatum = struct
  type t =
    { metric_name : String.t
    ; dimensions : Dimensions.t
    ; timestamp : DateTime.t option
    ; value : Double.t option
    ; statistic_values : StatisticSet.t option
    ; unit : StandardUnit.t option
    }

  let make ~metric_name ?(dimensions = []) ?timestamp ?value ?statistic_values ?unit () =
    { metric_name; dimensions; timestamp; value; statistic_values; unit }

  let parse xml =
    Some
      { metric_name =
          Xml.required
            "MetricName"
            (Util.option_bind (Xml.member "MetricName" xml) String.parse)
      ; dimensions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Dimensions" xml) Dimensions.parse)
      ; timestamp = Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse
      ; value = Util.option_bind (Xml.member "Value" xml) Double.parse
      ; statistic_values =
          Util.option_bind (Xml.member "StatisticValues" xml) StatisticSet.parse
      ; unit = Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.unit (fun f -> Query.Pair ("Unit", StandardUnit.to_query f))
         ; Util.option_map v.statistic_values (fun f ->
               Query.Pair ("StatisticValues", StatisticSet.to_query f))
         ; Util.option_map v.value (fun f -> Query.Pair ("Value", Double.to_query f))
         ; Util.option_map v.timestamp (fun f ->
               Query.Pair ("Timestamp", DateTime.to_query f))
         ; Some (Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Some (Query.Pair ("MetricName", String.to_query v.metric_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.unit (fun f -> "unit", StandardUnit.to_json f)
         ; Util.option_map v.statistic_values (fun f ->
               "statistic_values", StatisticSet.to_json f)
         ; Util.option_map v.value (fun f -> "value", Double.to_json f)
         ; Util.option_map v.timestamp (fun f -> "timestamp", DateTime.to_json f)
         ; Some ("dimensions", Dimensions.to_json v.dimensions)
         ; Some ("metric_name", String.to_json v.metric_name)
         ])

  let of_json j =
    { metric_name = String.of_json (Util.of_option_exn (Json.lookup j "metric_name"))
    ; dimensions = Dimensions.of_json (Util.of_option_exn (Json.lookup j "dimensions"))
    ; timestamp = Util.option_map (Json.lookup j "timestamp") DateTime.of_json
    ; value = Util.option_map (Json.lookup j "value") Double.of_json
    ; statistic_values =
        Util.option_map (Json.lookup j "statistic_values") StatisticSet.of_json
    ; unit = Util.option_map (Json.lookup j "unit") StandardUnit.of_json
    }
end

module Datapoint = struct
  type t =
    { timestamp : DateTime.t option
    ; sample_count : Double.t option
    ; average : Double.t option
    ; sum : Double.t option
    ; minimum : Double.t option
    ; maximum : Double.t option
    ; unit : StandardUnit.t option
    }

  let make ?timestamp ?sample_count ?average ?sum ?minimum ?maximum ?unit () =
    { timestamp; sample_count; average; sum; minimum; maximum; unit }

  let parse xml =
    Some
      { timestamp = Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse
      ; sample_count = Util.option_bind (Xml.member "SampleCount" xml) Double.parse
      ; average = Util.option_bind (Xml.member "Average" xml) Double.parse
      ; sum = Util.option_bind (Xml.member "Sum" xml) Double.parse
      ; minimum = Util.option_bind (Xml.member "Minimum" xml) Double.parse
      ; maximum = Util.option_bind (Xml.member "Maximum" xml) Double.parse
      ; unit = Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.unit (fun f -> Query.Pair ("Unit", StandardUnit.to_query f))
         ; Util.option_map v.maximum (fun f -> Query.Pair ("Maximum", Double.to_query f))
         ; Util.option_map v.minimum (fun f -> Query.Pair ("Minimum", Double.to_query f))
         ; Util.option_map v.sum (fun f -> Query.Pair ("Sum", Double.to_query f))
         ; Util.option_map v.average (fun f -> Query.Pair ("Average", Double.to_query f))
         ; Util.option_map v.sample_count (fun f ->
               Query.Pair ("SampleCount", Double.to_query f))
         ; Util.option_map v.timestamp (fun f ->
               Query.Pair ("Timestamp", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.unit (fun f -> "unit", StandardUnit.to_json f)
         ; Util.option_map v.maximum (fun f -> "maximum", Double.to_json f)
         ; Util.option_map v.minimum (fun f -> "minimum", Double.to_json f)
         ; Util.option_map v.sum (fun f -> "sum", Double.to_json f)
         ; Util.option_map v.average (fun f -> "average", Double.to_json f)
         ; Util.option_map v.sample_count (fun f -> "sample_count", Double.to_json f)
         ; Util.option_map v.timestamp (fun f -> "timestamp", DateTime.to_json f)
         ])

  let of_json j =
    { timestamp = Util.option_map (Json.lookup j "timestamp") DateTime.of_json
    ; sample_count = Util.option_map (Json.lookup j "sample_count") Double.of_json
    ; average = Util.option_map (Json.lookup j "average") Double.of_json
    ; sum = Util.option_map (Json.lookup j "sum") Double.of_json
    ; minimum = Util.option_map (Json.lookup j "minimum") Double.of_json
    ; maximum = Util.option_map (Json.lookup j "maximum") Double.of_json
    ; unit = Util.option_map (Json.lookup j "unit") StandardUnit.of_json
    }
end

module AlarmHistoryItems = struct
  type t = AlarmHistoryItem.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map AlarmHistoryItem.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list AlarmHistoryItem.to_query v

  let to_json v = `List (List.map AlarmHistoryItem.to_json v)

  let of_json j = Json.to_list AlarmHistoryItem.of_json j
end

module DimensionFilters = struct
  type t = DimensionFilter.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map DimensionFilter.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list DimensionFilter.to_query v

  let to_json v = `List (List.map DimensionFilter.to_json v)

  let of_json j = Json.to_list DimensionFilter.of_json j
end

module AlarmNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module MetricAlarms = struct
  type t = MetricAlarm.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map MetricAlarm.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list MetricAlarm.to_query v

  let to_json v = `List (List.map MetricAlarm.to_json v)

  let of_json j = Json.to_list MetricAlarm.of_json j
end

module Metrics = struct
  type t = Metric.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Metric.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Metric.to_query v

  let to_json v = `List (List.map Metric.to_json v)

  let of_json j = Json.to_list Metric.of_json j
end

module Statistics = struct
  type t = Statistic.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Statistic.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Statistic.to_query v

  let to_json v = `List (List.map Statistic.to_json v)

  let of_json j = Json.to_list Statistic.of_json j
end

module MetricData = struct
  type t = MetricDatum.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map MetricDatum.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list MetricDatum.to_query v

  let to_json v = `List (List.map MetricDatum.to_json v)

  let of_json j = Json.to_list MetricDatum.of_json j
end

module Datapoints = struct
  type t = Datapoint.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Datapoint.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Datapoint.to_query v

  let to_json v = `List (List.map Datapoint.to_json v)

  let of_json j = Json.to_list Datapoint.of_json j
end

module DescribeAlarmHistoryOutput = struct
  type t =
    { alarm_history_items : AlarmHistoryItems.t
    ; next_token : String.t option
    }

  let make ?(alarm_history_items = []) ?next_token () =
    { alarm_history_items; next_token }

  let parse xml =
    Some
      { alarm_history_items =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AlarmHistoryItems" xml)
               AlarmHistoryItems.parse)
      ; next_token = Util.option_bind (Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f ->
               Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Query.Pair
                ( "AlarmHistoryItems.member"
                , AlarmHistoryItems.to_query v.alarm_history_items ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f -> "next_token", String.to_json f)
         ; Some ("alarm_history_items", AlarmHistoryItems.to_json v.alarm_history_items)
         ])

  let of_json j =
    { alarm_history_items =
        AlarmHistoryItems.of_json
          (Util.of_option_exn (Json.lookup j "alarm_history_items"))
    ; next_token = Util.option_map (Json.lookup j "next_token") String.of_json
    }
end

module ListMetricsInput = struct
  type t =
    { namespace : String.t option
    ; metric_name : String.t option
    ; dimensions : DimensionFilters.t
    ; next_token : String.t option
    }

  let make ?namespace ?metric_name ?(dimensions = []) ?next_token () =
    { namespace; metric_name; dimensions; next_token }

  let parse xml =
    Some
      { namespace = Util.option_bind (Xml.member "Namespace" xml) String.parse
      ; metric_name = Util.option_bind (Xml.member "MetricName" xml) String.parse
      ; dimensions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Dimensions" xml) DimensionFilters.parse)
      ; next_token = Util.option_bind (Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f ->
               Query.Pair ("NextToken", String.to_query f))
         ; Some (Query.Pair ("Dimensions.member", DimensionFilters.to_query v.dimensions))
         ; Util.option_map v.metric_name (fun f ->
               Query.Pair ("MetricName", String.to_query f))
         ; Util.option_map v.namespace (fun f ->
               Query.Pair ("Namespace", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f -> "next_token", String.to_json f)
         ; Some ("dimensions", DimensionFilters.to_json v.dimensions)
         ; Util.option_map v.metric_name (fun f -> "metric_name", String.to_json f)
         ; Util.option_map v.namespace (fun f -> "namespace", String.to_json f)
         ])

  let of_json j =
    { namespace = Util.option_map (Json.lookup j "namespace") String.of_json
    ; metric_name = Util.option_map (Json.lookup j "metric_name") String.of_json
    ; dimensions =
        DimensionFilters.of_json (Util.of_option_exn (Json.lookup j "dimensions"))
    ; next_token = Util.option_map (Json.lookup j "next_token") String.of_json
    }
end

module InternalServiceFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "Message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module DescribeAlarmHistoryInput = struct
  type t =
    { alarm_name : String.t option
    ; history_item_type : HistoryItemType.t option
    ; start_date : DateTime.t option
    ; end_date : DateTime.t option
    ; max_records : Integer.t option
    ; next_token : String.t option
    }

  let make
      ?alarm_name
      ?history_item_type
      ?start_date
      ?end_date
      ?max_records
      ?next_token
      () =
    { alarm_name; history_item_type; start_date; end_date; max_records; next_token }

  let parse xml =
    Some
      { alarm_name = Util.option_bind (Xml.member "AlarmName" xml) String.parse
      ; history_item_type =
          Util.option_bind (Xml.member "HistoryItemType" xml) HistoryItemType.parse
      ; start_date = Util.option_bind (Xml.member "StartDate" xml) DateTime.parse
      ; end_date = Util.option_bind (Xml.member "EndDate" xml) DateTime.parse
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; next_token = Util.option_bind (Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f ->
               Query.Pair ("NextToken", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Util.option_map v.end_date (fun f ->
               Query.Pair ("EndDate", DateTime.to_query f))
         ; Util.option_map v.start_date (fun f ->
               Query.Pair ("StartDate", DateTime.to_query f))
         ; Util.option_map v.history_item_type (fun f ->
               Query.Pair ("HistoryItemType", HistoryItemType.to_query f))
         ; Util.option_map v.alarm_name (fun f ->
               Query.Pair ("AlarmName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f -> "next_token", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Util.option_map v.end_date (fun f -> "end_date", DateTime.to_json f)
         ; Util.option_map v.start_date (fun f -> "start_date", DateTime.to_json f)
         ; Util.option_map v.history_item_type (fun f ->
               "history_item_type", HistoryItemType.to_json f)
         ; Util.option_map v.alarm_name (fun f -> "alarm_name", String.to_json f)
         ])

  let of_json j =
    { alarm_name = Util.option_map (Json.lookup j "alarm_name") String.of_json
    ; history_item_type =
        Util.option_map (Json.lookup j "history_item_type") HistoryItemType.of_json
    ; start_date = Util.option_map (Json.lookup j "start_date") DateTime.of_json
    ; end_date = Util.option_map (Json.lookup j "end_date") DateTime.of_json
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; next_token = Util.option_map (Json.lookup j "next_token") String.of_json
    }
end

module DescribeAlarmsInput = struct
  type t =
    { alarm_names : AlarmNames.t
    ; alarm_name_prefix : String.t option
    ; state_value : StateValue.t option
    ; action_prefix : String.t option
    ; max_records : Integer.t option
    ; next_token : String.t option
    }

  let make
      ?(alarm_names = [])
      ?alarm_name_prefix
      ?state_value
      ?action_prefix
      ?max_records
      ?next_token
      () =
    { alarm_names
    ; alarm_name_prefix
    ; state_value
    ; action_prefix
    ; max_records
    ; next_token
    }

  let parse xml =
    Some
      { alarm_names =
          Util.of_option
            []
            (Util.option_bind (Xml.member "AlarmNames" xml) AlarmNames.parse)
      ; alarm_name_prefix =
          Util.option_bind (Xml.member "AlarmNamePrefix" xml) String.parse
      ; state_value = Util.option_bind (Xml.member "StateValue" xml) StateValue.parse
      ; action_prefix = Util.option_bind (Xml.member "ActionPrefix" xml) String.parse
      ; max_records = Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse
      ; next_token = Util.option_bind (Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f ->
               Query.Pair ("NextToken", String.to_query f))
         ; Util.option_map v.max_records (fun f ->
               Query.Pair ("MaxRecords", Integer.to_query f))
         ; Util.option_map v.action_prefix (fun f ->
               Query.Pair ("ActionPrefix", String.to_query f))
         ; Util.option_map v.state_value (fun f ->
               Query.Pair ("StateValue", StateValue.to_query f))
         ; Util.option_map v.alarm_name_prefix (fun f ->
               Query.Pair ("AlarmNamePrefix", String.to_query f))
         ; Some (Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f -> "next_token", String.to_json f)
         ; Util.option_map v.max_records (fun f -> "max_records", Integer.to_json f)
         ; Util.option_map v.action_prefix (fun f -> "action_prefix", String.to_json f)
         ; Util.option_map v.state_value (fun f -> "state_value", StateValue.to_json f)
         ; Util.option_map v.alarm_name_prefix (fun f ->
               "alarm_name_prefix", String.to_json f)
         ; Some ("alarm_names", AlarmNames.to_json v.alarm_names)
         ])

  let of_json j =
    { alarm_names = AlarmNames.of_json (Util.of_option_exn (Json.lookup j "alarm_names"))
    ; alarm_name_prefix =
        Util.option_map (Json.lookup j "alarm_name_prefix") String.of_json
    ; state_value = Util.option_map (Json.lookup j "state_value") StateValue.of_json
    ; action_prefix = Util.option_map (Json.lookup j "action_prefix") String.of_json
    ; max_records = Util.option_map (Json.lookup j "max_records") Integer.of_json
    ; next_token = Util.option_map (Json.lookup j "next_token") String.of_json
    }
end

module LimitExceededFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module PutMetricAlarmInput = struct
  type t =
    { alarm_name : String.t
    ; alarm_description : String.t option
    ; actions_enabled : Boolean.t option
    ; o_k_actions : ResourceList.t
    ; alarm_actions : ResourceList.t
    ; insufficient_data_actions : ResourceList.t
    ; metric_name : String.t
    ; namespace : String.t
    ; statistic : Statistic.t
    ; dimensions : Dimensions.t
    ; period : Integer.t
    ; unit : StandardUnit.t option
    ; evaluation_periods : Integer.t
    ; threshold : Double.t
    ; comparison_operator : ComparisonOperator.t
    }

  let make
      ~alarm_name
      ?alarm_description
      ?actions_enabled
      ?(o_k_actions = [])
      ?(alarm_actions = [])
      ?(insufficient_data_actions = [])
      ~metric_name
      ~namespace
      ~statistic
      ?(dimensions = [])
      ~period
      ?unit
      ~evaluation_periods
      ~threshold
      ~comparison_operator
      () =
    { alarm_name
    ; alarm_description
    ; actions_enabled
    ; o_k_actions
    ; alarm_actions
    ; insufficient_data_actions
    ; metric_name
    ; namespace
    ; statistic
    ; dimensions
    ; period
    ; unit
    ; evaluation_periods
    ; threshold
    ; comparison_operator
    }

  let parse xml =
    Some
      { alarm_name =
          Xml.required
            "AlarmName"
            (Util.option_bind (Xml.member "AlarmName" xml) String.parse)
      ; alarm_description =
          Util.option_bind (Xml.member "AlarmDescription" xml) String.parse
      ; actions_enabled = Util.option_bind (Xml.member "ActionsEnabled" xml) Boolean.parse
      ; o_k_actions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "OKActions" xml) ResourceList.parse)
      ; alarm_actions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "AlarmActions" xml) ResourceList.parse)
      ; insufficient_data_actions =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InsufficientDataActions" xml)
               ResourceList.parse)
      ; metric_name =
          Xml.required
            "MetricName"
            (Util.option_bind (Xml.member "MetricName" xml) String.parse)
      ; namespace =
          Xml.required
            "Namespace"
            (Util.option_bind (Xml.member "Namespace" xml) String.parse)
      ; statistic =
          Xml.required
            "Statistic"
            (Util.option_bind (Xml.member "Statistic" xml) Statistic.parse)
      ; dimensions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Dimensions" xml) Dimensions.parse)
      ; period =
          Xml.required "Period" (Util.option_bind (Xml.member "Period" xml) Integer.parse)
      ; unit = Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse
      ; evaluation_periods =
          Xml.required
            "EvaluationPeriods"
            (Util.option_bind (Xml.member "EvaluationPeriods" xml) Integer.parse)
      ; threshold =
          Xml.required
            "Threshold"
            (Util.option_bind (Xml.member "Threshold" xml) Double.parse)
      ; comparison_operator =
          Xml.required
            "ComparisonOperator"
            (Util.option_bind
               (Xml.member "ComparisonOperator" xml)
               ComparisonOperator.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("ComparisonOperator", ComparisonOperator.to_query v.comparison_operator))
         ; Some (Query.Pair ("Threshold", Double.to_query v.threshold))
         ; Some (Query.Pair ("EvaluationPeriods", Integer.to_query v.evaluation_periods))
         ; Util.option_map v.unit (fun f -> Query.Pair ("Unit", StandardUnit.to_query f))
         ; Some (Query.Pair ("Period", Integer.to_query v.period))
         ; Some (Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Some (Query.Pair ("Statistic", Statistic.to_query v.statistic))
         ; Some (Query.Pair ("Namespace", String.to_query v.namespace))
         ; Some (Query.Pair ("MetricName", String.to_query v.metric_name))
         ; Some
             (Query.Pair
                ( "InsufficientDataActions.member"
                , ResourceList.to_query v.insufficient_data_actions ))
         ; Some
             (Query.Pair ("AlarmActions.member", ResourceList.to_query v.alarm_actions))
         ; Some (Query.Pair ("OKActions.member", ResourceList.to_query v.o_k_actions))
         ; Util.option_map v.actions_enabled (fun f ->
               Query.Pair ("ActionsEnabled", Boolean.to_query f))
         ; Util.option_map v.alarm_description (fun f ->
               Query.Pair ("AlarmDescription", String.to_query f))
         ; Some (Query.Pair ("AlarmName", String.to_query v.alarm_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("comparison_operator", ComparisonOperator.to_json v.comparison_operator)
         ; Some ("threshold", Double.to_json v.threshold)
         ; Some ("evaluation_periods", Integer.to_json v.evaluation_periods)
         ; Util.option_map v.unit (fun f -> "unit", StandardUnit.to_json f)
         ; Some ("period", Integer.to_json v.period)
         ; Some ("dimensions", Dimensions.to_json v.dimensions)
         ; Some ("statistic", Statistic.to_json v.statistic)
         ; Some ("namespace", String.to_json v.namespace)
         ; Some ("metric_name", String.to_json v.metric_name)
         ; Some
             ( "insufficient_data_actions"
             , ResourceList.to_json v.insufficient_data_actions )
         ; Some ("alarm_actions", ResourceList.to_json v.alarm_actions)
         ; Some ("o_k_actions", ResourceList.to_json v.o_k_actions)
         ; Util.option_map v.actions_enabled (fun f ->
               "actions_enabled", Boolean.to_json f)
         ; Util.option_map v.alarm_description (fun f ->
               "alarm_description", String.to_json f)
         ; Some ("alarm_name", String.to_json v.alarm_name)
         ])

  let of_json j =
    { alarm_name = String.of_json (Util.of_option_exn (Json.lookup j "alarm_name"))
    ; alarm_description =
        Util.option_map (Json.lookup j "alarm_description") String.of_json
    ; actions_enabled = Util.option_map (Json.lookup j "actions_enabled") Boolean.of_json
    ; o_k_actions =
        ResourceList.of_json (Util.of_option_exn (Json.lookup j "o_k_actions"))
    ; alarm_actions =
        ResourceList.of_json (Util.of_option_exn (Json.lookup j "alarm_actions"))
    ; insufficient_data_actions =
        ResourceList.of_json
          (Util.of_option_exn (Json.lookup j "insufficient_data_actions"))
    ; metric_name = String.of_json (Util.of_option_exn (Json.lookup j "metric_name"))
    ; namespace = String.of_json (Util.of_option_exn (Json.lookup j "namespace"))
    ; statistic = Statistic.of_json (Util.of_option_exn (Json.lookup j "statistic"))
    ; dimensions = Dimensions.of_json (Util.of_option_exn (Json.lookup j "dimensions"))
    ; period = Integer.of_json (Util.of_option_exn (Json.lookup j "period"))
    ; unit = Util.option_map (Json.lookup j "unit") StandardUnit.of_json
    ; evaluation_periods =
        Integer.of_json (Util.of_option_exn (Json.lookup j "evaluation_periods"))
    ; threshold = Double.of_json (Util.of_option_exn (Json.lookup j "threshold"))
    ; comparison_operator =
        ComparisonOperator.of_json
          (Util.of_option_exn (Json.lookup j "comparison_operator"))
    }
end

module SetAlarmStateInput = struct
  type t =
    { alarm_name : String.t
    ; state_value : StateValue.t
    ; state_reason : String.t
    ; state_reason_data : String.t option
    }

  let make ~alarm_name ~state_value ~state_reason ?state_reason_data () =
    { alarm_name; state_value; state_reason; state_reason_data }

  let parse xml =
    Some
      { alarm_name =
          Xml.required
            "AlarmName"
            (Util.option_bind (Xml.member "AlarmName" xml) String.parse)
      ; state_value =
          Xml.required
            "StateValue"
            (Util.option_bind (Xml.member "StateValue" xml) StateValue.parse)
      ; state_reason =
          Xml.required
            "StateReason"
            (Util.option_bind (Xml.member "StateReason" xml) String.parse)
      ; state_reason_data =
          Util.option_bind (Xml.member "StateReasonData" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.state_reason_data (fun f ->
               Query.Pair ("StateReasonData", String.to_query f))
         ; Some (Query.Pair ("StateReason", String.to_query v.state_reason))
         ; Some (Query.Pair ("StateValue", StateValue.to_query v.state_value))
         ; Some (Query.Pair ("AlarmName", String.to_query v.alarm_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.state_reason_data (fun f ->
               "state_reason_data", String.to_json f)
         ; Some ("state_reason", String.to_json v.state_reason)
         ; Some ("state_value", StateValue.to_json v.state_value)
         ; Some ("alarm_name", String.to_json v.alarm_name)
         ])

  let of_json j =
    { alarm_name = String.of_json (Util.of_option_exn (Json.lookup j "alarm_name"))
    ; state_value = StateValue.of_json (Util.of_option_exn (Json.lookup j "state_value"))
    ; state_reason = String.of_json (Util.of_option_exn (Json.lookup j "state_reason"))
    ; state_reason_data =
        Util.option_map (Json.lookup j "state_reason_data") String.of_json
    }
end

module EnableAlarmActionsInput = struct
  type t = { alarm_names : AlarmNames.t }

  let make ~alarm_names () = { alarm_names }

  let parse xml =
    Some
      { alarm_names =
          Xml.required
            "AlarmNames"
            (Util.option_bind (Xml.member "AlarmNames" xml) AlarmNames.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("alarm_names", AlarmNames.to_json v.alarm_names) ])

  let of_json j =
    { alarm_names = AlarmNames.of_json (Util.of_option_exn (Json.lookup j "alarm_names"))
    }
end

module DescribeAlarmsForMetricInput = struct
  type t =
    { metric_name : String.t
    ; namespace : String.t
    ; statistic : Statistic.t option
    ; dimensions : Dimensions.t
    ; period : Integer.t option
    ; unit : StandardUnit.t option
    }

  let make ~metric_name ~namespace ?statistic ?(dimensions = []) ?period ?unit () =
    { metric_name; namespace; statistic; dimensions; period; unit }

  let parse xml =
    Some
      { metric_name =
          Xml.required
            "MetricName"
            (Util.option_bind (Xml.member "MetricName" xml) String.parse)
      ; namespace =
          Xml.required
            "Namespace"
            (Util.option_bind (Xml.member "Namespace" xml) String.parse)
      ; statistic = Util.option_bind (Xml.member "Statistic" xml) Statistic.parse
      ; dimensions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Dimensions" xml) Dimensions.parse)
      ; period = Util.option_bind (Xml.member "Period" xml) Integer.parse
      ; unit = Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.unit (fun f -> Query.Pair ("Unit", StandardUnit.to_query f))
         ; Util.option_map v.period (fun f -> Query.Pair ("Period", Integer.to_query f))
         ; Some (Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Util.option_map v.statistic (fun f ->
               Query.Pair ("Statistic", Statistic.to_query f))
         ; Some (Query.Pair ("Namespace", String.to_query v.namespace))
         ; Some (Query.Pair ("MetricName", String.to_query v.metric_name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.unit (fun f -> "unit", StandardUnit.to_json f)
         ; Util.option_map v.period (fun f -> "period", Integer.to_json f)
         ; Some ("dimensions", Dimensions.to_json v.dimensions)
         ; Util.option_map v.statistic (fun f -> "statistic", Statistic.to_json f)
         ; Some ("namespace", String.to_json v.namespace)
         ; Some ("metric_name", String.to_json v.metric_name)
         ])

  let of_json j =
    { metric_name = String.of_json (Util.of_option_exn (Json.lookup j "metric_name"))
    ; namespace = String.of_json (Util.of_option_exn (Json.lookup j "namespace"))
    ; statistic = Util.option_map (Json.lookup j "statistic") Statistic.of_json
    ; dimensions = Dimensions.of_json (Util.of_option_exn (Json.lookup j "dimensions"))
    ; period = Util.option_map (Json.lookup j "period") Integer.of_json
    ; unit = Util.option_map (Json.lookup j "unit") StandardUnit.of_json
    }
end

module InvalidParameterValueException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ResourceNotFound = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module DescribeAlarmsOutput = struct
  type t =
    { metric_alarms : MetricAlarms.t
    ; next_token : String.t option
    }

  let make ?(metric_alarms = []) ?next_token () = { metric_alarms; next_token }

  let parse xml =
    Some
      { metric_alarms =
          Util.of_option
            []
            (Util.option_bind (Xml.member "MetricAlarms" xml) MetricAlarms.parse)
      ; next_token = Util.option_bind (Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f ->
               Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Query.Pair ("MetricAlarms.member", MetricAlarms.to_query v.metric_alarms))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f -> "next_token", String.to_json f)
         ; Some ("metric_alarms", MetricAlarms.to_json v.metric_alarms)
         ])

  let of_json j =
    { metric_alarms =
        MetricAlarms.of_json (Util.of_option_exn (Json.lookup j "metric_alarms"))
    ; next_token = Util.option_map (Json.lookup j "next_token") String.of_json
    }
end

module InvalidNextToken = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module InvalidParameterCombinationException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module MissingRequiredParameterException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module ListMetricsOutput = struct
  type t =
    { metrics : Metrics.t
    ; next_token : String.t option
    }

  let make ?(metrics = []) ?next_token () = { metrics; next_token }

  let parse xml =
    Some
      { metrics =
          Util.of_option [] (Util.option_bind (Xml.member "Metrics" xml) Metrics.parse)
      ; next_token = Util.option_bind (Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f ->
               Query.Pair ("NextToken", String.to_query f))
         ; Some (Query.Pair ("Metrics.member", Metrics.to_query v.metrics))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.next_token (fun f -> "next_token", String.to_json f)
         ; Some ("metrics", Metrics.to_json v.metrics)
         ])

  let of_json j =
    { metrics = Metrics.of_json (Util.of_option_exn (Json.lookup j "metrics"))
    ; next_token = Util.option_map (Json.lookup j "next_token") String.of_json
    }
end

module InvalidFormatFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Util.option_bind (Xml.member "message" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j = { message = Util.option_map (Json.lookup j "message") String.of_json }
end

module GetMetricStatisticsInput = struct
  type t =
    { namespace : String.t
    ; metric_name : String.t
    ; dimensions : Dimensions.t
    ; start_time : DateTime.t
    ; end_time : DateTime.t
    ; period : Integer.t
    ; statistics : Statistics.t
    ; unit : StandardUnit.t option
    }

  let make
      ~namespace
      ~metric_name
      ?(dimensions = [])
      ~start_time
      ~end_time
      ~period
      ~statistics
      ?unit
      () =
    { namespace; metric_name; dimensions; start_time; end_time; period; statistics; unit }

  let parse xml =
    Some
      { namespace =
          Xml.required
            "Namespace"
            (Util.option_bind (Xml.member "Namespace" xml) String.parse)
      ; metric_name =
          Xml.required
            "MetricName"
            (Util.option_bind (Xml.member "MetricName" xml) String.parse)
      ; dimensions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Dimensions" xml) Dimensions.parse)
      ; start_time =
          Xml.required
            "StartTime"
            (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse)
      ; end_time =
          Xml.required
            "EndTime"
            (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse)
      ; period =
          Xml.required "Period" (Util.option_bind (Xml.member "Period" xml) Integer.parse)
      ; statistics =
          Xml.required
            "Statistics"
            (Util.option_bind (Xml.member "Statistics" xml) Statistics.parse)
      ; unit = Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.unit (fun f -> Query.Pair ("Unit", StandardUnit.to_query f))
         ; Some (Query.Pair ("Statistics.member", Statistics.to_query v.statistics))
         ; Some (Query.Pair ("Period", Integer.to_query v.period))
         ; Some (Query.Pair ("EndTime", DateTime.to_query v.end_time))
         ; Some (Query.Pair ("StartTime", DateTime.to_query v.start_time))
         ; Some (Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Some (Query.Pair ("MetricName", String.to_query v.metric_name))
         ; Some (Query.Pair ("Namespace", String.to_query v.namespace))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.unit (fun f -> "unit", StandardUnit.to_json f)
         ; Some ("statistics", Statistics.to_json v.statistics)
         ; Some ("period", Integer.to_json v.period)
         ; Some ("end_time", DateTime.to_json v.end_time)
         ; Some ("start_time", DateTime.to_json v.start_time)
         ; Some ("dimensions", Dimensions.to_json v.dimensions)
         ; Some ("metric_name", String.to_json v.metric_name)
         ; Some ("namespace", String.to_json v.namespace)
         ])

  let of_json j =
    { namespace = String.of_json (Util.of_option_exn (Json.lookup j "namespace"))
    ; metric_name = String.of_json (Util.of_option_exn (Json.lookup j "metric_name"))
    ; dimensions = Dimensions.of_json (Util.of_option_exn (Json.lookup j "dimensions"))
    ; start_time = DateTime.of_json (Util.of_option_exn (Json.lookup j "start_time"))
    ; end_time = DateTime.of_json (Util.of_option_exn (Json.lookup j "end_time"))
    ; period = Integer.of_json (Util.of_option_exn (Json.lookup j "period"))
    ; statistics = Statistics.of_json (Util.of_option_exn (Json.lookup j "statistics"))
    ; unit = Util.option_map (Json.lookup j "unit") StandardUnit.of_json
    }
end

module PutMetricDataInput = struct
  type t =
    { namespace : String.t
    ; metric_data : MetricData.t
    }

  let make ~namespace ~metric_data () = { namespace; metric_data }

  let parse xml =
    Some
      { namespace =
          Xml.required
            "Namespace"
            (Util.option_bind (Xml.member "Namespace" xml) String.parse)
      ; metric_data =
          Xml.required
            "MetricData"
            (Util.option_bind (Xml.member "MetricData" xml) MetricData.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("MetricData.member", MetricData.to_query v.metric_data))
         ; Some (Query.Pair ("Namespace", String.to_query v.namespace))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("metric_data", MetricData.to_json v.metric_data)
         ; Some ("namespace", String.to_json v.namespace)
         ])

  let of_json j =
    { namespace = String.of_json (Util.of_option_exn (Json.lookup j "namespace"))
    ; metric_data = MetricData.of_json (Util.of_option_exn (Json.lookup j "metric_data"))
    }
end

module DescribeAlarmsForMetricOutput = struct
  type t = { metric_alarms : MetricAlarms.t }

  let make ?(metric_alarms = []) () = { metric_alarms }

  let parse xml =
    Some
      { metric_alarms =
          Util.of_option
            []
            (Util.option_bind (Xml.member "MetricAlarms" xml) MetricAlarms.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("MetricAlarms.member", MetricAlarms.to_query v.metric_alarms))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("metric_alarms", MetricAlarms.to_json v.metric_alarms) ])

  let of_json j =
    { metric_alarms =
        MetricAlarms.of_json (Util.of_option_exn (Json.lookup j "metric_alarms"))
    }
end

module DeleteAlarmsInput = struct
  type t = { alarm_names : AlarmNames.t }

  let make ~alarm_names () = { alarm_names }

  let parse xml =
    Some
      { alarm_names =
          Xml.required
            "AlarmNames"
            (Util.option_bind (Xml.member "AlarmNames" xml) AlarmNames.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("alarm_names", AlarmNames.to_json v.alarm_names) ])

  let of_json j =
    { alarm_names = AlarmNames.of_json (Util.of_option_exn (Json.lookup j "alarm_names"))
    }
end

module DisableAlarmActionsInput = struct
  type t = { alarm_names : AlarmNames.t }

  let make ~alarm_names () = { alarm_names }

  let parse xml =
    Some
      { alarm_names =
          Xml.required
            "AlarmNames"
            (Util.option_bind (Xml.member "AlarmNames" xml) AlarmNames.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("alarm_names", AlarmNames.to_json v.alarm_names) ])

  let of_json j =
    { alarm_names = AlarmNames.of_json (Util.of_option_exn (Json.lookup j "alarm_names"))
    }
end

module GetMetricStatisticsOutput = struct
  type t =
    { label : String.t option
    ; datapoints : Datapoints.t
    }

  let make ?label ?(datapoints = []) () = { label; datapoints }

  let parse xml =
    Some
      { label = Util.option_bind (Xml.member "Label" xml) String.parse
      ; datapoints =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Datapoints" xml) Datapoints.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Datapoints.member", Datapoints.to_query v.datapoints))
         ; Util.option_map v.label (fun f -> Query.Pair ("Label", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("datapoints", Datapoints.to_json v.datapoints)
         ; Util.option_map v.label (fun f -> "label", String.to_json f)
         ])

  let of_json j =
    { label = Util.option_map (Json.lookup j "label") String.of_json
    ; datapoints = Datapoints.of_json (Util.of_option_exn (Json.lookup j "datapoints"))
    }
end
