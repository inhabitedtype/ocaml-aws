open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DatapointValueMap = struct
  type t = (String.t, Double.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v = Aws.Query.to_query_hashtbl String.to_string Double.to_query v

  let to_json v =
    `Assoc
      (Hashtbl.fold (fun k v acc -> (String.to_string k, Double.to_json v) :: acc) v [])

  let of_json j = Aws.Json.to_hashtbl String.of_string Double.of_json j
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
    ; extended_statistics : DatapointValueMap.t option
    }

  let make
      ?timestamp
      ?sample_count
      ?average
      ?sum
      ?minimum
      ?maximum
      ?unit
      ?extended_statistics
      () =
    { timestamp; sample_count; average; sum; minimum; maximum; unit; extended_statistics }

  let parse xml =
    Some
      { timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse
      ; sample_count =
          Aws.Util.option_bind (Aws.Xml.member "SampleCount" xml) Double.parse
      ; average = Aws.Util.option_bind (Aws.Xml.member "Average" xml) Double.parse
      ; sum = Aws.Util.option_bind (Aws.Xml.member "Sum" xml) Double.parse
      ; minimum = Aws.Util.option_bind (Aws.Xml.member "Minimum" xml) Double.parse
      ; maximum = Aws.Util.option_bind (Aws.Xml.member "Maximum" xml) Double.parse
      ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
      ; extended_statistics =
          Aws.Util.option_bind
            (Aws.Xml.member "ExtendedStatistics" xml)
            DatapointValueMap.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.extended_statistics (fun f ->
               Aws.Query.Pair ("ExtendedStatistics", DatapointValueMap.to_query f))
         ; Aws.Util.option_map v.unit (fun f ->
               Aws.Query.Pair ("Unit", StandardUnit.to_query f))
         ; Aws.Util.option_map v.maximum (fun f ->
               Aws.Query.Pair ("Maximum", Double.to_query f))
         ; Aws.Util.option_map v.minimum (fun f ->
               Aws.Query.Pair ("Minimum", Double.to_query f))
         ; Aws.Util.option_map v.sum (fun f -> Aws.Query.Pair ("Sum", Double.to_query f))
         ; Aws.Util.option_map v.average (fun f ->
               Aws.Query.Pair ("Average", Double.to_query f))
         ; Aws.Util.option_map v.sample_count (fun f ->
               Aws.Query.Pair ("SampleCount", Double.to_query f))
         ; Aws.Util.option_map v.timestamp (fun f ->
               Aws.Query.Pair ("Timestamp", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.extended_statistics (fun f ->
               "ExtendedStatistics", DatapointValueMap.to_json f)
         ; Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
         ; Aws.Util.option_map v.maximum (fun f -> "Maximum", Double.to_json f)
         ; Aws.Util.option_map v.minimum (fun f -> "Minimum", Double.to_json f)
         ; Aws.Util.option_map v.sum (fun f -> "Sum", Double.to_json f)
         ; Aws.Util.option_map v.average (fun f -> "Average", Double.to_json f)
         ; Aws.Util.option_map v.sample_count (fun f -> "SampleCount", Double.to_json f)
         ; Aws.Util.option_map v.timestamp (fun f -> "Timestamp", DateTime.to_json f)
         ])

  let of_json j =
    { timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") DateTime.of_json
    ; sample_count = Aws.Util.option_map (Aws.Json.lookup j "SampleCount") Double.of_json
    ; average = Aws.Util.option_map (Aws.Json.lookup j "Average") Double.of_json
    ; sum = Aws.Util.option_map (Aws.Json.lookup j "Sum") Double.of_json
    ; minimum = Aws.Util.option_map (Aws.Json.lookup j "Minimum") Double.of_json
    ; maximum = Aws.Util.option_map (Aws.Json.lookup j "Maximum") Double.of_json
    ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
    ; extended_statistics =
        Aws.Util.option_map
          (Aws.Json.lookup j "ExtendedStatistics")
          DatapointValueMap.of_json
    }
end

module Datapoints = struct
  type t = Datapoint.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Datapoint.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Datapoint.to_query v

  let to_json v = `List (List.map Datapoint.to_json v)

  let of_json j = Aws.Json.to_list Datapoint.of_json j
end

module GetMetricStatisticsOutput = struct
  type t =
    { label : String.t option
    ; datapoints : Datapoints.t
    }

  let make ?label ?(datapoints = []) () = { label; datapoints }

  let parse xml =
    Some
      { label = Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse
      ; datapoints =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Datapoints" xml) Datapoints.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Datapoints.member", Datapoints.to_query v.datapoints))
         ; Aws.Util.option_map v.label (fun f ->
               Aws.Query.Pair ("Label", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Datapoints", Datapoints.to_json v.datapoints)
         ; Aws.Util.option_map v.label (fun f -> "Label", String.to_json f)
         ])

  let of_json j =
    { label = Aws.Util.option_map (Aws.Json.lookup j "Label") String.of_json
    ; datapoints =
        Datapoints.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Datapoints"))
    }
end

module AlarmNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DisableAlarmActionsInput = struct
  type t = { alarm_names : AlarmNames.t }

  let make ~alarm_names () = { alarm_names }

  let parse xml =
    Some
      { alarm_names =
          Aws.Xml.required
            "AlarmNames"
            (Aws.Util.option_bind (Aws.Xml.member "AlarmNames" xml) AlarmNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("AlarmNames", AlarmNames.to_json v.alarm_names) ])

  let of_json j =
    { alarm_names =
        AlarmNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmNames"))
    }
end

module DashboardNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DeleteDashboardsInput = struct
  type t = { dashboard_names : DashboardNames.t }

  let make ~dashboard_names () = { dashboard_names }

  let parse xml =
    Some
      { dashboard_names =
          Aws.Xml.required
            "DashboardNames"
            (Aws.Util.option_bind
               (Aws.Xml.member "DashboardNames" xml)
               DashboardNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("DashboardNames.member", DashboardNames.to_query v.dashboard_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("DashboardNames", DashboardNames.to_json v.dashboard_names) ])

  let of_json j =
    { dashboard_names =
        DashboardNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardNames"))
    }
end

module Values = struct
  type t = Double.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Double.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Double.to_query v

  let to_json v = `List (List.map Double.to_json v)

  let of_json j = Aws.Json.to_list Double.of_json j
end

module Dimension = struct
  type t =
    { name : String.t
    ; value : String.t
    }

  let make ~name ~value () = { name; value }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Value", String.to_json v.value); Some ("Name", String.to_json v.name) ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    }
end

module Dimensions = struct
  type t = Dimension.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Dimension.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Dimension.to_query v

  let to_json v = `List (List.map Dimension.to_json v)

  let of_json j = Aws.Json.to_list Dimension.of_json j
end

module Range = struct
  type t =
    { start_time : DateTime.t
    ; end_time : DateTime.t
    }

  let make ~start_time ~end_time () = { start_time; end_time }

  let parse xml =
    Some
      { start_time =
          Aws.Xml.required
            "StartTime"
            (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
      ; end_time =
          Aws.Xml.required
            "EndTime"
            (Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("EndTime", DateTime.to_query v.end_time))
         ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("EndTime", DateTime.to_json v.end_time)
         ; Some ("StartTime", DateTime.to_json v.start_time)
         ])

  let of_json j =
    { start_time =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
    ; end_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndTime"))
    }
end

module AnomalyDetectorExcludedTimeRanges = struct
  type t = Range.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Range.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Range.to_query v

  let to_json v = `List (List.map Range.to_json v)

  let of_json j = Aws.Json.to_list Range.of_json j
end

module AnomalyDetectorConfiguration = struct
  type t =
    { excluded_time_ranges : AnomalyDetectorExcludedTimeRanges.t
    ; metric_timezone : String.t option
    }

  let make ?(excluded_time_ranges = []) ?metric_timezone () =
    { excluded_time_ranges; metric_timezone }

  let parse xml =
    Some
      { excluded_time_ranges =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ExcludedTimeRanges" xml)
               AnomalyDetectorExcludedTimeRanges.parse)
      ; metric_timezone =
          Aws.Util.option_bind (Aws.Xml.member "MetricTimezone" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.metric_timezone (fun f ->
               Aws.Query.Pair ("MetricTimezone", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ExcludedTimeRanges.member"
                , AnomalyDetectorExcludedTimeRanges.to_query v.excluded_time_ranges ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.metric_timezone (fun f ->
               "MetricTimezone", String.to_json f)
         ; Some
             ( "ExcludedTimeRanges"
             , AnomalyDetectorExcludedTimeRanges.to_json v.excluded_time_ranges )
         ])

  let of_json j =
    { excluded_time_ranges =
        AnomalyDetectorExcludedTimeRanges.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludedTimeRanges"))
    ; metric_timezone =
        Aws.Util.option_map (Aws.Json.lookup j "MetricTimezone") String.of_json
    }
end

module PutAnomalyDetectorInput = struct
  type t =
    { namespace : String.t
    ; metric_name : String.t
    ; dimensions : Dimensions.t
    ; stat : String.t
    ; configuration : AnomalyDetectorConfiguration.t option
    }

  let make ~namespace ~metric_name ?(dimensions = []) ~stat ?configuration () =
    { namespace; metric_name; dimensions; stat; configuration }

  let parse xml =
    Some
      { namespace =
          Aws.Xml.required
            "Namespace"
            (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
      ; metric_name =
          Aws.Xml.required
            "MetricName"
            (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      ; stat =
          Aws.Xml.required
            "Stat"
            (Aws.Util.option_bind (Aws.Xml.member "Stat" xml) String.parse)
      ; configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "Configuration" xml)
            AnomalyDetectorConfiguration.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.configuration (fun f ->
               Aws.Query.Pair ("Configuration", AnomalyDetectorConfiguration.to_query f))
         ; Some (Aws.Query.Pair ("Stat", String.to_query v.stat))
         ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
         ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.configuration (fun f ->
               "Configuration", AnomalyDetectorConfiguration.to_json f)
         ; Some ("Stat", String.to_json v.stat)
         ; Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Some ("MetricName", String.to_json v.metric_name)
         ; Some ("Namespace", String.to_json v.namespace)
         ])

  let of_json j =
    { namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
    ; metric_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    ; stat = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Stat"))
    ; configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "Configuration")
          AnomalyDetectorConfiguration.of_json
    }
end

module DatapointValues = struct
  type t = Double.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Double.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Double.to_query v

  let to_json v = `List (List.map Double.to_json v)

  let of_json j = Aws.Json.to_list Double.of_json j
end

module Tag = struct
  type t =
    { key : String.t
    ; value : String.t
    }

  let make ~key ~value () = { key; value }

  let parse xml =
    Some
      { key =
          Aws.Xml.required
            "Key"
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Value", String.to_json v.value); Some ("Key", String.to_json v.key) ])

  let of_json j =
    { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    }
end

module TagList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Aws.Json.to_list Tag.of_json j
end

module ListTagsForResourceOutput = struct
  type t = { tags : TagList.t }

  let make ?(tags = []) () = { tags }

  let parse xml =
    Some
      { tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Tags", TagList.to_json v.tags) ])

  let of_json j =
    { tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags")) }
end

module InsightRuleContributorKeys = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module Timestamps = struct
  type t = DateTime.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map DateTime.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list DateTime.to_query v

  let to_json v = `List (List.map DateTime.to_json v)

  let of_json j = Aws.Json.to_list DateTime.of_json j
end

module StatusCode = struct
  type t =
    | Complete
    | InternalError
    | PartialData

  let str_to_t =
    [ "PartialData", PartialData; "InternalError", InternalError; "Complete", Complete ]

  let t_to_str =
    [ PartialData, "PartialData"; InternalError, "InternalError"; Complete, "Complete" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module MessageData = struct
  type t =
    { code : String.t option
    ; value : String.t option
    }

  let make ?code ?value () = { code; value }

  let parse xml =
    Some
      { code = Aws.Util.option_bind (Aws.Xml.member "Code" xml) String.parse
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", String.to_query f))
         ; Aws.Util.option_map v.code (fun f ->
               Aws.Query.Pair ("Code", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
         ; Aws.Util.option_map v.code (fun f -> "Code", String.to_json f)
         ])

  let of_json j =
    { code = Aws.Util.option_map (Aws.Json.lookup j "Code") String.of_json
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
    }
end

module MetricDataResultMessages = struct
  type t = MessageData.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map MessageData.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list MessageData.to_query v

  let to_json v = `List (List.map MessageData.to_json v)

  let of_json j = Aws.Json.to_list MessageData.of_json j
end

module MetricDataResult = struct
  type t =
    { id : String.t option
    ; label : String.t option
    ; timestamps : Timestamps.t
    ; values : DatapointValues.t
    ; status_code : StatusCode.t option
    ; messages : MetricDataResultMessages.t
    }

  let make ?id ?label ?(timestamps = []) ?(values = []) ?status_code ?(messages = []) () =
    { id; label; timestamps; values; status_code; messages }

  let parse xml =
    Some
      { id = Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse
      ; label = Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse
      ; timestamps =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Timestamps" xml) Timestamps.parse)
      ; values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Values" xml) DatapointValues.parse)
      ; status_code =
          Aws.Util.option_bind (Aws.Xml.member "StatusCode" xml) StatusCode.parse
      ; messages =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Messages" xml)
               MetricDataResultMessages.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("Messages.member", MetricDataResultMessages.to_query v.messages))
         ; Aws.Util.option_map v.status_code (fun f ->
               Aws.Query.Pair ("StatusCode", StatusCode.to_query f))
         ; Some (Aws.Query.Pair ("Values.member", DatapointValues.to_query v.values))
         ; Some (Aws.Query.Pair ("Timestamps.member", Timestamps.to_query v.timestamps))
         ; Aws.Util.option_map v.label (fun f ->
               Aws.Query.Pair ("Label", String.to_query f))
         ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Messages", MetricDataResultMessages.to_json v.messages)
         ; Aws.Util.option_map v.status_code (fun f -> "StatusCode", StatusCode.to_json f)
         ; Some ("Values", DatapointValues.to_json v.values)
         ; Some ("Timestamps", Timestamps.to_json v.timestamps)
         ; Aws.Util.option_map v.label (fun f -> "Label", String.to_json f)
         ; Aws.Util.option_map v.id (fun f -> "Id", String.to_json f)
         ])

  let of_json j =
    { id = Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json
    ; label = Aws.Util.option_map (Aws.Json.lookup j "Label") String.of_json
    ; timestamps =
        Timestamps.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamps"))
    ; values =
        DatapointValues.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
    ; status_code =
        Aws.Util.option_map (Aws.Json.lookup j "StatusCode") StatusCode.of_json
    ; messages =
        MetricDataResultMessages.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Messages"))
    }
end

module DescribeAnomalyDetectorsInput = struct
  type t =
    { next_token : String.t option
    ; max_results : Integer.t option
    ; namespace : String.t option
    ; metric_name : String.t option
    ; dimensions : Dimensions.t
    }

  let make ?next_token ?max_results ?namespace ?metric_name ?(dimensions = []) () =
    { next_token; max_results; namespace; metric_name; dimensions }

  let parse xml =
    Some
      { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      ; namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
      ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Aws.Util.option_map v.metric_name (fun f ->
               Aws.Query.Pair ("MetricName", String.to_query f))
         ; Aws.Util.option_map v.namespace (fun f ->
               Aws.Query.Pair ("Namespace", String.to_query f))
         ; Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
         ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
         ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ])

  let of_json j =
    { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    ; namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
    ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    }
end

module DeleteAlarmsInput = struct
  type t = { alarm_names : AlarmNames.t }

  let make ~alarm_names () = { alarm_names }

  let parse xml =
    Some
      { alarm_names =
          Aws.Xml.required
            "AlarmNames"
            (Aws.Util.option_bind (Aws.Xml.member "AlarmNames" xml) AlarmNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("AlarmNames", AlarmNames.to_json v.alarm_names) ])

  let of_json j =
    { alarm_names =
        AlarmNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmNames"))
    }
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StateValue = struct
  type t =
    | OK
    | ALARM
    | INSUFFICIENT_DATA

  let str_to_t = [ "INSUFFICIENT_DATA", INSUFFICIENT_DATA; "ALARM", ALARM; "OK", OK ]

  let t_to_str = [ INSUFFICIENT_DATA, "INSUFFICIENT_DATA"; ALARM, "ALARM"; OK, "OK" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ResourceList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
      { namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
      ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Aws.Util.option_map v.metric_name (fun f ->
               Aws.Query.Pair ("MetricName", String.to_query f))
         ; Aws.Util.option_map v.namespace (fun f ->
               Aws.Query.Pair ("Namespace", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
         ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
         ])

  let of_json j =
    { namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
    ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    }
end

module MetricStat = struct
  type t =
    { metric : Metric.t
    ; period : Integer.t
    ; stat : String.t
    ; unit : StandardUnit.t option
    }

  let make ~metric ~period ~stat ?unit () = { metric; period; stat; unit }

  let parse xml =
    Some
      { metric =
          Aws.Xml.required
            "Metric"
            (Aws.Util.option_bind (Aws.Xml.member "Metric" xml) Metric.parse)
      ; period =
          Aws.Xml.required
            "Period"
            (Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse)
      ; stat =
          Aws.Xml.required
            "Stat"
            (Aws.Util.option_bind (Aws.Xml.member "Stat" xml) String.parse)
      ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.unit (fun f ->
               Aws.Query.Pair ("Unit", StandardUnit.to_query f))
         ; Some (Aws.Query.Pair ("Stat", String.to_query v.stat))
         ; Some (Aws.Query.Pair ("Period", Integer.to_query v.period))
         ; Some (Aws.Query.Pair ("Metric", Metric.to_query v.metric))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
         ; Some ("Stat", String.to_json v.stat)
         ; Some ("Period", Integer.to_json v.period)
         ; Some ("Metric", Metric.to_json v.metric)
         ])

  let of_json j =
    { metric = Metric.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metric"))
    ; period = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Period"))
    ; stat = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Stat"))
    ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
    }
end

module MetricDataQuery = struct
  type t =
    { id : String.t
    ; metric_stat : MetricStat.t option
    ; expression : String.t option
    ; label : String.t option
    ; return_data : Boolean.t option
    ; period : Integer.t option
    }

  let make ~id ?metric_stat ?expression ?label ?return_data ?period () =
    { id; metric_stat; expression; label; return_data; period }

  let parse xml =
    Some
      { id =
          Aws.Xml.required
            "Id"
            (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
      ; metric_stat =
          Aws.Util.option_bind (Aws.Xml.member "MetricStat" xml) MetricStat.parse
      ; expression = Aws.Util.option_bind (Aws.Xml.member "Expression" xml) String.parse
      ; label = Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse
      ; return_data = Aws.Util.option_bind (Aws.Xml.member "ReturnData" xml) Boolean.parse
      ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.period (fun f ->
               Aws.Query.Pair ("Period", Integer.to_query f))
         ; Aws.Util.option_map v.return_data (fun f ->
               Aws.Query.Pair ("ReturnData", Boolean.to_query f))
         ; Aws.Util.option_map v.label (fun f ->
               Aws.Query.Pair ("Label", String.to_query f))
         ; Aws.Util.option_map v.expression (fun f ->
               Aws.Query.Pair ("Expression", String.to_query f))
         ; Aws.Util.option_map v.metric_stat (fun f ->
               Aws.Query.Pair ("MetricStat", MetricStat.to_query f))
         ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
         ; Aws.Util.option_map v.return_data (fun f -> "ReturnData", Boolean.to_json f)
         ; Aws.Util.option_map v.label (fun f -> "Label", String.to_json f)
         ; Aws.Util.option_map v.expression (fun f -> "Expression", String.to_json f)
         ; Aws.Util.option_map v.metric_stat (fun f -> "MetricStat", MetricStat.to_json f)
         ; Some ("Id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
    ; metric_stat =
        Aws.Util.option_map (Aws.Json.lookup j "MetricStat") MetricStat.of_json
    ; expression = Aws.Util.option_map (Aws.Json.lookup j "Expression") String.of_json
    ; label = Aws.Util.option_map (Aws.Json.lookup j "Label") String.of_json
    ; return_data = Aws.Util.option_map (Aws.Json.lookup j "ReturnData") Boolean.of_json
    ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
    }
end

module MetricDataQueries = struct
  type t = MetricDataQuery.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map MetricDataQuery.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list MetricDataQuery.to_query v

  let to_json v = `List (List.map MetricDataQuery.to_json v)

  let of_json j = Aws.Json.to_list MetricDataQuery.of_json j
end

module ComparisonOperator = struct
  type t =
    | GreaterThanOrEqualToThreshold
    | GreaterThanThreshold
    | LessThanThreshold
    | LessThanOrEqualToThreshold
    | LessThanLowerOrGreaterThanUpperThreshold
    | LessThanLowerThreshold
    | GreaterThanUpperThreshold

  let str_to_t =
    [ "GreaterThanUpperThreshold", GreaterThanUpperThreshold
    ; "LessThanLowerThreshold", LessThanLowerThreshold
    ; "LessThanLowerOrGreaterThanUpperThreshold", LessThanLowerOrGreaterThanUpperThreshold
    ; "LessThanOrEqualToThreshold", LessThanOrEqualToThreshold
    ; "LessThanThreshold", LessThanThreshold
    ; "GreaterThanThreshold", GreaterThanThreshold
    ; "GreaterThanOrEqualToThreshold", GreaterThanOrEqualToThreshold
    ]

  let t_to_str =
    [ GreaterThanUpperThreshold, "GreaterThanUpperThreshold"
    ; LessThanLowerThreshold, "LessThanLowerThreshold"
    ; LessThanLowerOrGreaterThanUpperThreshold, "LessThanLowerOrGreaterThanUpperThreshold"
    ; LessThanOrEqualToThreshold, "LessThanOrEqualToThreshold"
    ; LessThanThreshold, "LessThanThreshold"
    ; GreaterThanThreshold, "GreaterThanThreshold"
    ; GreaterThanOrEqualToThreshold, "GreaterThanOrEqualToThreshold"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
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
    ; extended_statistic : String.t option
    ; dimensions : Dimensions.t
    ; period : Integer.t option
    ; unit : StandardUnit.t option
    ; evaluation_periods : Integer.t option
    ; datapoints_to_alarm : Integer.t option
    ; threshold : Double.t option
    ; comparison_operator : ComparisonOperator.t option
    ; treat_missing_data : String.t option
    ; evaluate_low_sample_count_percentile : String.t option
    ; metrics : MetricDataQueries.t
    ; threshold_metric_id : String.t option
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
      ?extended_statistic
      ?(dimensions = [])
      ?period
      ?unit
      ?evaluation_periods
      ?datapoints_to_alarm
      ?threshold
      ?comparison_operator
      ?treat_missing_data
      ?evaluate_low_sample_count_percentile
      ?(metrics = [])
      ?threshold_metric_id
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
    ; extended_statistic
    ; dimensions
    ; period
    ; unit
    ; evaluation_periods
    ; datapoints_to_alarm
    ; threshold
    ; comparison_operator
    ; treat_missing_data
    ; evaluate_low_sample_count_percentile
    ; metrics
    ; threshold_metric_id
    }

  let parse xml =
    Some
      { alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
      ; alarm_arn = Aws.Util.option_bind (Aws.Xml.member "AlarmArn" xml) String.parse
      ; alarm_description =
          Aws.Util.option_bind (Aws.Xml.member "AlarmDescription" xml) String.parse
      ; alarm_configuration_updated_timestamp =
          Aws.Util.option_bind
            (Aws.Xml.member "AlarmConfigurationUpdatedTimestamp" xml)
            DateTime.parse
      ; actions_enabled =
          Aws.Util.option_bind (Aws.Xml.member "ActionsEnabled" xml) Boolean.parse
      ; o_k_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "OKActions" xml) ResourceList.parse)
      ; alarm_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "AlarmActions" xml) ResourceList.parse)
      ; insufficient_data_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "InsufficientDataActions" xml)
               ResourceList.parse)
      ; state_value =
          Aws.Util.option_bind (Aws.Xml.member "StateValue" xml) StateValue.parse
      ; state_reason =
          Aws.Util.option_bind (Aws.Xml.member "StateReason" xml) String.parse
      ; state_reason_data =
          Aws.Util.option_bind (Aws.Xml.member "StateReasonData" xml) String.parse
      ; state_updated_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "StateUpdatedTimestamp" xml) DateTime.parse
      ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
      ; namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
      ; statistic = Aws.Util.option_bind (Aws.Xml.member "Statistic" xml) Statistic.parse
      ; extended_statistic =
          Aws.Util.option_bind (Aws.Xml.member "ExtendedStatistic" xml) String.parse
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
      ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
      ; evaluation_periods =
          Aws.Util.option_bind (Aws.Xml.member "EvaluationPeriods" xml) Integer.parse
      ; datapoints_to_alarm =
          Aws.Util.option_bind (Aws.Xml.member "DatapointsToAlarm" xml) Integer.parse
      ; threshold = Aws.Util.option_bind (Aws.Xml.member "Threshold" xml) Double.parse
      ; comparison_operator =
          Aws.Util.option_bind
            (Aws.Xml.member "ComparisonOperator" xml)
            ComparisonOperator.parse
      ; treat_missing_data =
          Aws.Util.option_bind (Aws.Xml.member "TreatMissingData" xml) String.parse
      ; evaluate_low_sample_count_percentile =
          Aws.Util.option_bind
            (Aws.Xml.member "EvaluateLowSampleCountPercentile" xml)
            String.parse
      ; metrics =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml) MetricDataQueries.parse)
      ; threshold_metric_id =
          Aws.Util.option_bind (Aws.Xml.member "ThresholdMetricId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.threshold_metric_id (fun f ->
               Aws.Query.Pair ("ThresholdMetricId", String.to_query f))
         ; Some (Aws.Query.Pair ("Metrics.member", MetricDataQueries.to_query v.metrics))
         ; Aws.Util.option_map v.evaluate_low_sample_count_percentile (fun f ->
               Aws.Query.Pair ("EvaluateLowSampleCountPercentile", String.to_query f))
         ; Aws.Util.option_map v.treat_missing_data (fun f ->
               Aws.Query.Pair ("TreatMissingData", String.to_query f))
         ; Aws.Util.option_map v.comparison_operator (fun f ->
               Aws.Query.Pair ("ComparisonOperator", ComparisonOperator.to_query f))
         ; Aws.Util.option_map v.threshold (fun f ->
               Aws.Query.Pair ("Threshold", Double.to_query f))
         ; Aws.Util.option_map v.datapoints_to_alarm (fun f ->
               Aws.Query.Pair ("DatapointsToAlarm", Integer.to_query f))
         ; Aws.Util.option_map v.evaluation_periods (fun f ->
               Aws.Query.Pair ("EvaluationPeriods", Integer.to_query f))
         ; Aws.Util.option_map v.unit (fun f ->
               Aws.Query.Pair ("Unit", StandardUnit.to_query f))
         ; Aws.Util.option_map v.period (fun f ->
               Aws.Query.Pair ("Period", Integer.to_query f))
         ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Aws.Util.option_map v.extended_statistic (fun f ->
               Aws.Query.Pair ("ExtendedStatistic", String.to_query f))
         ; Aws.Util.option_map v.statistic (fun f ->
               Aws.Query.Pair ("Statistic", Statistic.to_query f))
         ; Aws.Util.option_map v.namespace (fun f ->
               Aws.Query.Pair ("Namespace", String.to_query f))
         ; Aws.Util.option_map v.metric_name (fun f ->
               Aws.Query.Pair ("MetricName", String.to_query f))
         ; Aws.Util.option_map v.state_updated_timestamp (fun f ->
               Aws.Query.Pair ("StateUpdatedTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.state_reason_data (fun f ->
               Aws.Query.Pair ("StateReasonData", String.to_query f))
         ; Aws.Util.option_map v.state_reason (fun f ->
               Aws.Query.Pair ("StateReason", String.to_query f))
         ; Aws.Util.option_map v.state_value (fun f ->
               Aws.Query.Pair ("StateValue", StateValue.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "InsufficientDataActions.member"
                , ResourceList.to_query v.insufficient_data_actions ))
         ; Some
             (Aws.Query.Pair ("AlarmActions.member", ResourceList.to_query v.alarm_actions))
         ; Some (Aws.Query.Pair ("OKActions.member", ResourceList.to_query v.o_k_actions))
         ; Aws.Util.option_map v.actions_enabled (fun f ->
               Aws.Query.Pair ("ActionsEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
               Aws.Query.Pair ("AlarmConfigurationUpdatedTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.alarm_description (fun f ->
               Aws.Query.Pair ("AlarmDescription", String.to_query f))
         ; Aws.Util.option_map v.alarm_arn (fun f ->
               Aws.Query.Pair ("AlarmArn", String.to_query f))
         ; Aws.Util.option_map v.alarm_name (fun f ->
               Aws.Query.Pair ("AlarmName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.threshold_metric_id (fun f ->
               "ThresholdMetricId", String.to_json f)
         ; Some ("Metrics", MetricDataQueries.to_json v.metrics)
         ; Aws.Util.option_map v.evaluate_low_sample_count_percentile (fun f ->
               "EvaluateLowSampleCountPercentile", String.to_json f)
         ; Aws.Util.option_map v.treat_missing_data (fun f ->
               "TreatMissingData", String.to_json f)
         ; Aws.Util.option_map v.comparison_operator (fun f ->
               "ComparisonOperator", ComparisonOperator.to_json f)
         ; Aws.Util.option_map v.threshold (fun f -> "Threshold", Double.to_json f)
         ; Aws.Util.option_map v.datapoints_to_alarm (fun f ->
               "DatapointsToAlarm", Integer.to_json f)
         ; Aws.Util.option_map v.evaluation_periods (fun f ->
               "EvaluationPeriods", Integer.to_json f)
         ; Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
         ; Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
         ; Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Aws.Util.option_map v.extended_statistic (fun f ->
               "ExtendedStatistic", String.to_json f)
         ; Aws.Util.option_map v.statistic (fun f -> "Statistic", Statistic.to_json f)
         ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
         ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
         ; Aws.Util.option_map v.state_updated_timestamp (fun f ->
               "StateUpdatedTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.state_reason_data (fun f ->
               "StateReasonData", String.to_json f)
         ; Aws.Util.option_map v.state_reason (fun f -> "StateReason", String.to_json f)
         ; Aws.Util.option_map v.state_value (fun f -> "StateValue", StateValue.to_json f)
         ; Some
             ("InsufficientDataActions", ResourceList.to_json v.insufficient_data_actions)
         ; Some ("AlarmActions", ResourceList.to_json v.alarm_actions)
         ; Some ("OKActions", ResourceList.to_json v.o_k_actions)
         ; Aws.Util.option_map v.actions_enabled (fun f ->
               "ActionsEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
               "AlarmConfigurationUpdatedTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.alarm_description (fun f ->
               "AlarmDescription", String.to_json f)
         ; Aws.Util.option_map v.alarm_arn (fun f -> "AlarmArn", String.to_json f)
         ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
         ])

  let of_json j =
    { alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
    ; alarm_arn = Aws.Util.option_map (Aws.Json.lookup j "AlarmArn") String.of_json
    ; alarm_description =
        Aws.Util.option_map (Aws.Json.lookup j "AlarmDescription") String.of_json
    ; alarm_configuration_updated_timestamp =
        Aws.Util.option_map
          (Aws.Json.lookup j "AlarmConfigurationUpdatedTimestamp")
          DateTime.of_json
    ; actions_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "ActionsEnabled") Boolean.of_json
    ; o_k_actions =
        ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OKActions"))
    ; alarm_actions =
        ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmActions"))
    ; insufficient_data_actions =
        ResourceList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "InsufficientDataActions"))
    ; state_value =
        Aws.Util.option_map (Aws.Json.lookup j "StateValue") StateValue.of_json
    ; state_reason = Aws.Util.option_map (Aws.Json.lookup j "StateReason") String.of_json
    ; state_reason_data =
        Aws.Util.option_map (Aws.Json.lookup j "StateReasonData") String.of_json
    ; state_updated_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "StateUpdatedTimestamp") DateTime.of_json
    ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
    ; namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
    ; statistic = Aws.Util.option_map (Aws.Json.lookup j "Statistic") Statistic.of_json
    ; extended_statistic =
        Aws.Util.option_map (Aws.Json.lookup j "ExtendedStatistic") String.of_json
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
    ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
    ; evaluation_periods =
        Aws.Util.option_map (Aws.Json.lookup j "EvaluationPeriods") Integer.of_json
    ; datapoints_to_alarm =
        Aws.Util.option_map (Aws.Json.lookup j "DatapointsToAlarm") Integer.of_json
    ; threshold = Aws.Util.option_map (Aws.Json.lookup j "Threshold") Double.of_json
    ; comparison_operator =
        Aws.Util.option_map
          (Aws.Json.lookup j "ComparisonOperator")
          ComparisonOperator.of_json
    ; treat_missing_data =
        Aws.Util.option_map (Aws.Json.lookup j "TreatMissingData") String.of_json
    ; evaluate_low_sample_count_percentile =
        Aws.Util.option_map
          (Aws.Json.lookup j "EvaluateLowSampleCountPercentile")
          String.of_json
    ; metrics =
        MetricDataQueries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
    ; threshold_metric_id =
        Aws.Util.option_map (Aws.Json.lookup j "ThresholdMetricId") String.of_json
    }
end

module MetricAlarms = struct
  type t = MetricAlarm.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map MetricAlarm.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list MetricAlarm.to_query v

  let to_json v = `List (List.map MetricAlarm.to_json v)

  let of_json j = Aws.Json.to_list MetricAlarm.of_json j
end

module DescribeAlarmsForMetricOutput = struct
  type t = { metric_alarms : MetricAlarms.t }

  let make ?(metric_alarms = []) () = { metric_alarms }

  let parse xml =
    Some
      { metric_alarms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "MetricAlarms" xml) MetricAlarms.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("MetricAlarms.member", MetricAlarms.to_query v.metric_alarms))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MetricAlarms", MetricAlarms.to_json v.metric_alarms) ])

  let of_json j =
    { metric_alarms =
        MetricAlarms.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricAlarms"))
    }
end

module AnomalyDetectorStateValue = struct
  type t =
    | PENDING_TRAINING
    | TRAINED_INSUFFICIENT_DATA
    | TRAINED

  let str_to_t =
    [ "TRAINED", TRAINED
    ; "TRAINED_INSUFFICIENT_DATA", TRAINED_INSUFFICIENT_DATA
    ; "PENDING_TRAINING", PENDING_TRAINING
    ]

  let t_to_str =
    [ TRAINED, "TRAINED"
    ; TRAINED_INSUFFICIENT_DATA, "TRAINED_INSUFFICIENT_DATA"
    ; PENDING_TRAINING, "PENDING_TRAINING"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AnomalyDetector = struct
  type t =
    { namespace : String.t option
    ; metric_name : String.t option
    ; dimensions : Dimensions.t
    ; stat : String.t option
    ; configuration : AnomalyDetectorConfiguration.t option
    ; state_value : AnomalyDetectorStateValue.t option
    }

  let make ?namespace ?metric_name ?(dimensions = []) ?stat ?configuration ?state_value ()
      =
    { namespace; metric_name; dimensions; stat; configuration; state_value }

  let parse xml =
    Some
      { namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
      ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      ; stat = Aws.Util.option_bind (Aws.Xml.member "Stat" xml) String.parse
      ; configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "Configuration" xml)
            AnomalyDetectorConfiguration.parse
      ; state_value =
          Aws.Util.option_bind
            (Aws.Xml.member "StateValue" xml)
            AnomalyDetectorStateValue.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.state_value (fun f ->
               Aws.Query.Pair ("StateValue", AnomalyDetectorStateValue.to_query f))
         ; Aws.Util.option_map v.configuration (fun f ->
               Aws.Query.Pair ("Configuration", AnomalyDetectorConfiguration.to_query f))
         ; Aws.Util.option_map v.stat (fun f ->
               Aws.Query.Pair ("Stat", String.to_query f))
         ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Aws.Util.option_map v.metric_name (fun f ->
               Aws.Query.Pair ("MetricName", String.to_query f))
         ; Aws.Util.option_map v.namespace (fun f ->
               Aws.Query.Pair ("Namespace", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.state_value (fun f ->
               "StateValue", AnomalyDetectorStateValue.to_json f)
         ; Aws.Util.option_map v.configuration (fun f ->
               "Configuration", AnomalyDetectorConfiguration.to_json f)
         ; Aws.Util.option_map v.stat (fun f -> "Stat", String.to_json f)
         ; Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
         ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
         ])

  let of_json j =
    { namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
    ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    ; stat = Aws.Util.option_map (Aws.Json.lookup j "Stat") String.of_json
    ; configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "Configuration")
          AnomalyDetectorConfiguration.of_json
    ; state_value =
        Aws.Util.option_map
          (Aws.Json.lookup j "StateValue")
          AnomalyDetectorStateValue.of_json
    }
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
          Aws.Xml.required
            "SampleCount"
            (Aws.Util.option_bind (Aws.Xml.member "SampleCount" xml) Double.parse)
      ; sum =
          Aws.Xml.required
            "Sum"
            (Aws.Util.option_bind (Aws.Xml.member "Sum" xml) Double.parse)
      ; minimum =
          Aws.Xml.required
            "Minimum"
            (Aws.Util.option_bind (Aws.Xml.member "Minimum" xml) Double.parse)
      ; maximum =
          Aws.Xml.required
            "Maximum"
            (Aws.Util.option_bind (Aws.Xml.member "Maximum" xml) Double.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Maximum", Double.to_query v.maximum))
         ; Some (Aws.Query.Pair ("Minimum", Double.to_query v.minimum))
         ; Some (Aws.Query.Pair ("Sum", Double.to_query v.sum))
         ; Some (Aws.Query.Pair ("SampleCount", Double.to_query v.sample_count))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Maximum", Double.to_json v.maximum)
         ; Some ("Minimum", Double.to_json v.minimum)
         ; Some ("Sum", Double.to_json v.sum)
         ; Some ("SampleCount", Double.to_json v.sample_count)
         ])

  let of_json j =
    { sample_count =
        Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SampleCount"))
    ; sum = Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Sum"))
    ; minimum = Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Minimum"))
    ; maximum = Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Maximum"))
    }
end

module Counts = struct
  type t = Double.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Double.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Double.to_query v

  let to_json v = `List (List.map Double.to_json v)

  let of_json j = Aws.Json.to_list Double.of_json j
end

module MetricDatum = struct
  type t =
    { metric_name : String.t
    ; dimensions : Dimensions.t
    ; timestamp : DateTime.t option
    ; value : Double.t option
    ; statistic_values : StatisticSet.t option
    ; values : Values.t
    ; counts : Counts.t
    ; unit : StandardUnit.t option
    ; storage_resolution : Integer.t option
    }

  let make
      ~metric_name
      ?(dimensions = [])
      ?timestamp
      ?value
      ?statistic_values
      ?(values = [])
      ?(counts = [])
      ?unit
      ?storage_resolution
      () =
    { metric_name
    ; dimensions
    ; timestamp
    ; value
    ; statistic_values
    ; values
    ; counts
    ; unit
    ; storage_resolution
    }

  let parse xml =
    Some
      { metric_name =
          Aws.Xml.required
            "MetricName"
            (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      ; timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) Double.parse
      ; statistic_values =
          Aws.Util.option_bind (Aws.Xml.member "StatisticValues" xml) StatisticSet.parse
      ; values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Values" xml) Values.parse)
      ; counts =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Counts" xml) Counts.parse)
      ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
      ; storage_resolution =
          Aws.Util.option_bind (Aws.Xml.member "StorageResolution" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.storage_resolution (fun f ->
               Aws.Query.Pair ("StorageResolution", Integer.to_query f))
         ; Aws.Util.option_map v.unit (fun f ->
               Aws.Query.Pair ("Unit", StandardUnit.to_query f))
         ; Some (Aws.Query.Pair ("Counts.member", Counts.to_query v.counts))
         ; Some (Aws.Query.Pair ("Values.member", Values.to_query v.values))
         ; Aws.Util.option_map v.statistic_values (fun f ->
               Aws.Query.Pair ("StatisticValues", StatisticSet.to_query f))
         ; Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", Double.to_query f))
         ; Aws.Util.option_map v.timestamp (fun f ->
               Aws.Query.Pair ("Timestamp", DateTime.to_query f))
         ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.storage_resolution (fun f ->
               "StorageResolution", Integer.to_json f)
         ; Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
         ; Some ("Counts", Counts.to_json v.counts)
         ; Some ("Values", Values.to_json v.values)
         ; Aws.Util.option_map v.statistic_values (fun f ->
               "StatisticValues", StatisticSet.to_json f)
         ; Aws.Util.option_map v.value (fun f -> "Value", Double.to_json f)
         ; Aws.Util.option_map v.timestamp (fun f -> "Timestamp", DateTime.to_json f)
         ; Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Some ("MetricName", String.to_json v.metric_name)
         ])

  let of_json j =
    { metric_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") DateTime.of_json
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") Double.of_json
    ; statistic_values =
        Aws.Util.option_map (Aws.Json.lookup j "StatisticValues") StatisticSet.of_json
    ; values = Values.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
    ; counts = Counts.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Counts"))
    ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
    ; storage_resolution =
        Aws.Util.option_map (Aws.Json.lookup j "StorageResolution") Integer.of_json
    }
end

module MetricData = struct
  type t = MetricDatum.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map MetricDatum.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list MetricDatum.to_query v

  let to_json v = `List (List.map MetricDatum.to_json v)

  let of_json j = Aws.Json.to_list MetricDatum.of_json j
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
          Aws.Xml.required
            "Namespace"
            (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
      ; metric_data =
          Aws.Xml.required
            "MetricData"
            (Aws.Util.option_bind (Aws.Xml.member "MetricData" xml) MetricData.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("MetricData.member", MetricData.to_query v.metric_data))
         ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("MetricData", MetricData.to_json v.metric_data)
         ; Some ("Namespace", String.to_json v.namespace)
         ])

  let of_json j =
    { namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
    ; metric_data =
        MetricData.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricData"))
    }
end

module Statistics = struct
  type t = Statistic.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Statistic.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Statistic.to_query v

  let to_json v = `List (List.map Statistic.to_json v)

  let of_json j = Aws.Json.to_list Statistic.of_json j
end

module ExtendedStatistics = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
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
    ; extended_statistics : ExtendedStatistics.t
    ; unit : StandardUnit.t option
    }

  let make
      ~namespace
      ~metric_name
      ?(dimensions = [])
      ~start_time
      ~end_time
      ~period
      ?(statistics = [])
      ?(extended_statistics = [])
      ?unit
      () =
    { namespace
    ; metric_name
    ; dimensions
    ; start_time
    ; end_time
    ; period
    ; statistics
    ; extended_statistics
    ; unit
    }

  let parse xml =
    Some
      { namespace =
          Aws.Xml.required
            "Namespace"
            (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
      ; metric_name =
          Aws.Xml.required
            "MetricName"
            (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      ; start_time =
          Aws.Xml.required
            "StartTime"
            (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
      ; end_time =
          Aws.Xml.required
            "EndTime"
            (Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse)
      ; period =
          Aws.Xml.required
            "Period"
            (Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse)
      ; statistics =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Statistics" xml) Statistics.parse)
      ; extended_statistics =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ExtendedStatistics" xml)
               ExtendedStatistics.parse)
      ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.unit (fun f ->
               Aws.Query.Pair ("Unit", StandardUnit.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ExtendedStatistics.member"
                , ExtendedStatistics.to_query v.extended_statistics ))
         ; Some (Aws.Query.Pair ("Statistics.member", Statistics.to_query v.statistics))
         ; Some (Aws.Query.Pair ("Period", Integer.to_query v.period))
         ; Some (Aws.Query.Pair ("EndTime", DateTime.to_query v.end_time))
         ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
         ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
         ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
         ; Some ("ExtendedStatistics", ExtendedStatistics.to_json v.extended_statistics)
         ; Some ("Statistics", Statistics.to_json v.statistics)
         ; Some ("Period", Integer.to_json v.period)
         ; Some ("EndTime", DateTime.to_json v.end_time)
         ; Some ("StartTime", DateTime.to_json v.start_time)
         ; Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Some ("MetricName", String.to_json v.metric_name)
         ; Some ("Namespace", String.to_json v.namespace)
         ])

  let of_json j =
    { namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
    ; metric_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    ; start_time =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
    ; end_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndTime"))
    ; period = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Period"))
    ; statistics =
        Statistics.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Statistics"))
    ; extended_statistics =
        ExtendedStatistics.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ExtendedStatistics"))
    ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
    }
end

module PutCompositeAlarmInput = struct
  type t =
    { actions_enabled : Boolean.t option
    ; alarm_actions : ResourceList.t
    ; alarm_description : String.t option
    ; alarm_name : String.t
    ; alarm_rule : String.t
    ; insufficient_data_actions : ResourceList.t
    ; o_k_actions : ResourceList.t
    ; tags : TagList.t
    }

  let make
      ?actions_enabled
      ?(alarm_actions = [])
      ?alarm_description
      ~alarm_name
      ~alarm_rule
      ?(insufficient_data_actions = [])
      ?(o_k_actions = [])
      ?(tags = [])
      () =
    { actions_enabled
    ; alarm_actions
    ; alarm_description
    ; alarm_name
    ; alarm_rule
    ; insufficient_data_actions
    ; o_k_actions
    ; tags
    }

  let parse xml =
    Some
      { actions_enabled =
          Aws.Util.option_bind (Aws.Xml.member "ActionsEnabled" xml) Boolean.parse
      ; alarm_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "AlarmActions" xml) ResourceList.parse)
      ; alarm_description =
          Aws.Util.option_bind (Aws.Xml.member "AlarmDescription" xml) String.parse
      ; alarm_name =
          Aws.Xml.required
            "AlarmName"
            (Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse)
      ; alarm_rule =
          Aws.Xml.required
            "AlarmRule"
            (Aws.Util.option_bind (Aws.Xml.member "AlarmRule" xml) String.parse)
      ; insufficient_data_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "InsufficientDataActions" xml)
               ResourceList.parse)
      ; o_k_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "OKActions" xml) ResourceList.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Aws.Query.Pair ("OKActions.member", ResourceList.to_query v.o_k_actions))
         ; Some
             (Aws.Query.Pair
                ( "InsufficientDataActions.member"
                , ResourceList.to_query v.insufficient_data_actions ))
         ; Some (Aws.Query.Pair ("AlarmRule", String.to_query v.alarm_rule))
         ; Some (Aws.Query.Pair ("AlarmName", String.to_query v.alarm_name))
         ; Aws.Util.option_map v.alarm_description (fun f ->
               Aws.Query.Pair ("AlarmDescription", String.to_query f))
         ; Some
             (Aws.Query.Pair ("AlarmActions.member", ResourceList.to_query v.alarm_actions))
         ; Aws.Util.option_map v.actions_enabled (fun f ->
               Aws.Query.Pair ("ActionsEnabled", Boolean.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Some ("OKActions", ResourceList.to_json v.o_k_actions)
         ; Some
             ("InsufficientDataActions", ResourceList.to_json v.insufficient_data_actions)
         ; Some ("AlarmRule", String.to_json v.alarm_rule)
         ; Some ("AlarmName", String.to_json v.alarm_name)
         ; Aws.Util.option_map v.alarm_description (fun f ->
               "AlarmDescription", String.to_json f)
         ; Some ("AlarmActions", ResourceList.to_json v.alarm_actions)
         ; Aws.Util.option_map v.actions_enabled (fun f ->
               "ActionsEnabled", Boolean.to_json f)
         ])

  let of_json j =
    { actions_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "ActionsEnabled") Boolean.of_json
    ; alarm_actions =
        ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmActions"))
    ; alarm_description =
        Aws.Util.option_map (Aws.Json.lookup j "AlarmDescription") String.of_json
    ; alarm_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmName"))
    ; alarm_rule = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmRule"))
    ; insufficient_data_actions =
        ResourceList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "InsufficientDataActions"))
    ; o_k_actions =
        ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OKActions"))
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module InsightRule = struct
  type t =
    { name : String.t
    ; state : String.t
    ; schema : String.t
    ; definition : String.t
    }

  let make ~name ~state ~schema ~definition () = { name; state; schema; definition }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; state =
          Aws.Xml.required
            "State"
            (Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse)
      ; schema =
          Aws.Xml.required
            "Schema"
            (Aws.Util.option_bind (Aws.Xml.member "Schema" xml) String.parse)
      ; definition =
          Aws.Xml.required
            "Definition"
            (Aws.Util.option_bind (Aws.Xml.member "Definition" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Definition", String.to_query v.definition))
         ; Some (Aws.Query.Pair ("Schema", String.to_query v.schema))
         ; Some (Aws.Query.Pair ("State", String.to_query v.state))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Definition", String.to_json v.definition)
         ; Some ("Schema", String.to_json v.schema)
         ; Some ("State", String.to_json v.state)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; state = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "State"))
    ; schema = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Schema"))
    ; definition =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Definition"))
    }
end

module InsightRules = struct
  type t = InsightRule.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map InsightRule.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list InsightRule.to_query v

  let to_json v = `List (List.map InsightRule.to_json v)

  let of_json j = Aws.Json.to_list InsightRule.of_json j
end

module DescribeInsightRulesOutput = struct
  type t =
    { next_token : String.t option
    ; insight_rules : InsightRules.t
    }

  let make ?next_token ?(insight_rules = []) () = { next_token; insight_rules }

  let parse xml =
    Some
      { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; insight_rules =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "InsightRules" xml) InsightRules.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("InsightRules.member", InsightRules.to_query v.insight_rules))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("InsightRules", InsightRules.to_json v.insight_rules)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ])

  let of_json j =
    { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; insight_rules =
        InsightRules.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InsightRules"))
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
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", String.to_query f))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
    }
end

module DashboardNotFoundError = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module InvalidFormatFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
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

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DeleteAnomalyDetectorOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module PartialFailure = struct
  type t =
    { failure_resource : String.t option
    ; exception_type : String.t option
    ; failure_code : String.t option
    ; failure_description : String.t option
    }

  let make ?failure_resource ?exception_type ?failure_code ?failure_description () =
    { failure_resource; exception_type; failure_code; failure_description }

  let parse xml =
    Some
      { failure_resource =
          Aws.Util.option_bind (Aws.Xml.member "FailureResource" xml) String.parse
      ; exception_type =
          Aws.Util.option_bind (Aws.Xml.member "ExceptionType" xml) String.parse
      ; failure_code =
          Aws.Util.option_bind (Aws.Xml.member "FailureCode" xml) String.parse
      ; failure_description =
          Aws.Util.option_bind (Aws.Xml.member "FailureDescription" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.failure_description (fun f ->
               Aws.Query.Pair ("FailureDescription", String.to_query f))
         ; Aws.Util.option_map v.failure_code (fun f ->
               Aws.Query.Pair ("FailureCode", String.to_query f))
         ; Aws.Util.option_map v.exception_type (fun f ->
               Aws.Query.Pair ("ExceptionType", String.to_query f))
         ; Aws.Util.option_map v.failure_resource (fun f ->
               Aws.Query.Pair ("FailureResource", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.failure_description (fun f ->
               "FailureDescription", String.to_json f)
         ; Aws.Util.option_map v.failure_code (fun f -> "FailureCode", String.to_json f)
         ; Aws.Util.option_map v.exception_type (fun f ->
               "ExceptionType", String.to_json f)
         ; Aws.Util.option_map v.failure_resource (fun f ->
               "FailureResource", String.to_json f)
         ])

  let of_json j =
    { failure_resource =
        Aws.Util.option_map (Aws.Json.lookup j "FailureResource") String.of_json
    ; exception_type =
        Aws.Util.option_map (Aws.Json.lookup j "ExceptionType") String.of_json
    ; failure_code = Aws.Util.option_map (Aws.Json.lookup j "FailureCode") String.of_json
    ; failure_description =
        Aws.Util.option_map (Aws.Json.lookup j "FailureDescription") String.of_json
    }
end

module BatchFailures = struct
  type t = PartialFailure.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map PartialFailure.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list PartialFailure.to_query v

  let to_json v = `List (List.map PartialFailure.to_json v)

  let of_json j = Aws.Json.to_list PartialFailure.of_json j
end

module ResourceNotFoundException = struct
  type t =
    { resource_type : String.t option
    ; resource_id : String.t option
    }

  let make ?resource_type ?resource_id () = { resource_type; resource_id }

  let parse xml =
    Some
      { resource_type =
          Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
      ; resource_id = Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.resource_id (fun f ->
               Aws.Query.Pair ("ResourceId", String.to_query f))
         ; Aws.Util.option_map v.resource_type (fun f ->
               Aws.Query.Pair ("ResourceType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.resource_id (fun f -> "ResourceId", String.to_json f)
         ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
         ])

  let of_json j =
    { resource_type =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
    ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "ResourceId") String.of_json
    }
end

module PutInsightRuleOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module LimitExceededException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteInsightRulesOutput = struct
  type t = { failures : BatchFailures.t }

  let make ?(failures = []) () = { failures }

  let parse xml =
    Some
      { failures =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Failures" xml) BatchFailures.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Failures.member", BatchFailures.to_query v.failures)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Failures", BatchFailures.to_json v.failures) ])

  let of_json j =
    { failures =
        BatchFailures.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Failures"))
    }
end

module AlarmType = struct
  type t =
    | CompositeAlarm
    | MetricAlarm

  let str_to_t = [ "MetricAlarm", MetricAlarm; "CompositeAlarm", CompositeAlarm ]

  let t_to_str = [ MetricAlarm, "MetricAlarm"; CompositeAlarm, "CompositeAlarm" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AlarmTypes = struct
  type t = AlarmType.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map AlarmType.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AlarmType.to_query v

  let to_json v = `List (List.map AlarmType.to_json v)

  let of_json j = Aws.Json.to_list AlarmType.of_json j
end

module DeleteAnomalyDetectorInput = struct
  type t =
    { namespace : String.t
    ; metric_name : String.t
    ; dimensions : Dimensions.t
    ; stat : String.t
    }

  let make ~namespace ~metric_name ?(dimensions = []) ~stat () =
    { namespace; metric_name; dimensions; stat }

  let parse xml =
    Some
      { namespace =
          Aws.Xml.required
            "Namespace"
            (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
      ; metric_name =
          Aws.Xml.required
            "MetricName"
            (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      ; stat =
          Aws.Xml.required
            "Stat"
            (Aws.Util.option_bind (Aws.Xml.member "Stat" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Stat", String.to_query v.stat))
         ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
         ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Stat", String.to_json v.stat)
         ; Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Some ("MetricName", String.to_json v.metric_name)
         ; Some ("Namespace", String.to_json v.namespace)
         ])

  let of_json j =
    { namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
    ; metric_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    ; stat = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Stat"))
    }
end

module TagResourceOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module Metrics = struct
  type t = Metric.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Metric.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Metric.to_query v

  let to_json v = `List (List.map Metric.to_json v)

  let of_json j = Aws.Json.to_list Metric.of_json j
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
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml) Metrics.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Metrics.member", Metrics.to_query v.metrics))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Metrics", Metrics.to_json v.metrics)
         ])

  let of_json j =
    { metrics = Metrics.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DashboardValidationMessage = struct
  type t =
    { data_path : String.t option
    ; message : String.t option
    }

  let make ?data_path ?message () = { data_path; message }

  let parse xml =
    Some
      { data_path = Aws.Util.option_bind (Aws.Xml.member "DataPath" xml) String.parse
      ; message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("Message", String.to_query f))
         ; Aws.Util.option_map v.data_path (fun f ->
               Aws.Query.Pair ("DataPath", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
         ; Aws.Util.option_map v.data_path (fun f -> "DataPath", String.to_json f)
         ])

  let of_json j =
    { data_path = Aws.Util.option_map (Aws.Json.lookup j "DataPath") String.of_json
    ; message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
    }
end

module DashboardValidationMessages = struct
  type t = DashboardValidationMessage.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map DashboardValidationMessage.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list DashboardValidationMessage.to_query v

  let to_json v = `List (List.map DashboardValidationMessage.to_json v)

  let of_json j = Aws.Json.to_list DashboardValidationMessage.of_json j
end

module AlarmHistoryItem = struct
  type t =
    { alarm_name : String.t option
    ; alarm_type : AlarmType.t option
    ; timestamp : DateTime.t option
    ; history_item_type : HistoryItemType.t option
    ; history_summary : String.t option
    ; history_data : String.t option
    }

  let make
      ?alarm_name
      ?alarm_type
      ?timestamp
      ?history_item_type
      ?history_summary
      ?history_data
      () =
    { alarm_name
    ; alarm_type
    ; timestamp
    ; history_item_type
    ; history_summary
    ; history_data
    }

  let parse xml =
    Some
      { alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
      ; alarm_type = Aws.Util.option_bind (Aws.Xml.member "AlarmType" xml) AlarmType.parse
      ; timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse
      ; history_item_type =
          Aws.Util.option_bind
            (Aws.Xml.member "HistoryItemType" xml)
            HistoryItemType.parse
      ; history_summary =
          Aws.Util.option_bind (Aws.Xml.member "HistorySummary" xml) String.parse
      ; history_data =
          Aws.Util.option_bind (Aws.Xml.member "HistoryData" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.history_data (fun f ->
               Aws.Query.Pair ("HistoryData", String.to_query f))
         ; Aws.Util.option_map v.history_summary (fun f ->
               Aws.Query.Pair ("HistorySummary", String.to_query f))
         ; Aws.Util.option_map v.history_item_type (fun f ->
               Aws.Query.Pair ("HistoryItemType", HistoryItemType.to_query f))
         ; Aws.Util.option_map v.timestamp (fun f ->
               Aws.Query.Pair ("Timestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.alarm_type (fun f ->
               Aws.Query.Pair ("AlarmType", AlarmType.to_query f))
         ; Aws.Util.option_map v.alarm_name (fun f ->
               Aws.Query.Pair ("AlarmName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.history_data (fun f -> "HistoryData", String.to_json f)
         ; Aws.Util.option_map v.history_summary (fun f ->
               "HistorySummary", String.to_json f)
         ; Aws.Util.option_map v.history_item_type (fun f ->
               "HistoryItemType", HistoryItemType.to_json f)
         ; Aws.Util.option_map v.timestamp (fun f -> "Timestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.alarm_type (fun f -> "AlarmType", AlarmType.to_json f)
         ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
         ])

  let of_json j =
    { alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
    ; alarm_type = Aws.Util.option_map (Aws.Json.lookup j "AlarmType") AlarmType.of_json
    ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") DateTime.of_json
    ; history_item_type =
        Aws.Util.option_map (Aws.Json.lookup j "HistoryItemType") HistoryItemType.of_json
    ; history_summary =
        Aws.Util.option_map (Aws.Json.lookup j "HistorySummary") String.of_json
    ; history_data = Aws.Util.option_map (Aws.Json.lookup j "HistoryData") String.of_json
    }
end

module AlarmHistoryItems = struct
  type t = AlarmHistoryItem.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map AlarmHistoryItem.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AlarmHistoryItem.to_query v

  let to_json v = `List (List.map AlarmHistoryItem.to_json v)

  let of_json j = Aws.Json.to_list AlarmHistoryItem.of_json j
end

module PutDashboardOutput = struct
  type t = { dashboard_validation_messages : DashboardValidationMessages.t }

  let make ?(dashboard_validation_messages = []) () = { dashboard_validation_messages }

  let parse xml =
    Some
      { dashboard_validation_messages =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "DashboardValidationMessages" xml)
               DashboardValidationMessages.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "DashboardValidationMessages.member"
                , DashboardValidationMessages.to_query v.dashboard_validation_messages ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "DashboardValidationMessages"
             , DashboardValidationMessages.to_json v.dashboard_validation_messages )
         ])

  let of_json j =
    { dashboard_validation_messages =
        DashboardValidationMessages.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardValidationMessages"))
    }
end

module MissingRequiredParameterException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module InsightRuleContributorDatapoint = struct
  type t =
    { timestamp : DateTime.t
    ; approximate_value : Double.t
    }

  let make ~timestamp ~approximate_value () = { timestamp; approximate_value }

  let parse xml =
    Some
      { timestamp =
          Aws.Xml.required
            "Timestamp"
            (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
      ; approximate_value =
          Aws.Xml.required
            "ApproximateValue"
            (Aws.Util.option_bind (Aws.Xml.member "ApproximateValue" xml) Double.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ApproximateValue", Double.to_query v.approximate_value))
         ; Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ApproximateValue", Double.to_json v.approximate_value)
         ; Some ("Timestamp", DateTime.to_json v.timestamp)
         ])

  let of_json j =
    { timestamp =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
    ; approximate_value =
        Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApproximateValue"))
    }
end

module InsightRuleContributorDatapoints = struct
  type t = InsightRuleContributorDatapoint.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map InsightRuleContributorDatapoint.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list InsightRuleContributorDatapoint.to_query v

  let to_json v = `List (List.map InsightRuleContributorDatapoint.to_json v)

  let of_json j = Aws.Json.to_list InsightRuleContributorDatapoint.of_json j
end

module InsightRuleContributor = struct
  type t =
    { keys : InsightRuleContributorKeys.t
    ; approximate_aggregate_value : Double.t
    ; datapoints : InsightRuleContributorDatapoints.t
    }

  let make ~keys ~approximate_aggregate_value ~datapoints () =
    { keys; approximate_aggregate_value; datapoints }

  let parse xml =
    Some
      { keys =
          Aws.Xml.required
            "Keys"
            (Aws.Util.option_bind
               (Aws.Xml.member "Keys" xml)
               InsightRuleContributorKeys.parse)
      ; approximate_aggregate_value =
          Aws.Xml.required
            "ApproximateAggregateValue"
            (Aws.Util.option_bind
               (Aws.Xml.member "ApproximateAggregateValue" xml)
               Double.parse)
      ; datapoints =
          Aws.Xml.required
            "Datapoints"
            (Aws.Util.option_bind
               (Aws.Xml.member "Datapoints" xml)
               InsightRuleContributorDatapoints.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "Datapoints.member"
                , InsightRuleContributorDatapoints.to_query v.datapoints ))
         ; Some
             (Aws.Query.Pair
                ( "ApproximateAggregateValue"
                , Double.to_query v.approximate_aggregate_value ))
         ; Some
             (Aws.Query.Pair ("Keys.member", InsightRuleContributorKeys.to_query v.keys))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Datapoints", InsightRuleContributorDatapoints.to_json v.datapoints)
         ; Some ("ApproximateAggregateValue", Double.to_json v.approximate_aggregate_value)
         ; Some ("Keys", InsightRuleContributorKeys.to_json v.keys)
         ])

  let of_json j =
    { keys =
        InsightRuleContributorKeys.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Keys"))
    ; approximate_aggregate_value =
        Double.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ApproximateAggregateValue"))
    ; datapoints =
        InsightRuleContributorDatapoints.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Datapoints"))
    }
end

module AnomalyDetectors = struct
  type t = AnomalyDetector.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map AnomalyDetector.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AnomalyDetector.to_query v

  let to_json v = `List (List.map AnomalyDetector.to_json v)

  let of_json j = Aws.Json.to_list AnomalyDetector.of_json j
end

module InvalidParameterCombinationException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DashboardEntry = struct
  type t =
    { dashboard_name : String.t option
    ; dashboard_arn : String.t option
    ; last_modified : DateTime.t option
    ; size : Long.t option
    }

  let make ?dashboard_name ?dashboard_arn ?last_modified ?size () =
    { dashboard_name; dashboard_arn; last_modified; size }

  let parse xml =
    Some
      { dashboard_name =
          Aws.Util.option_bind (Aws.Xml.member "DashboardName" xml) String.parse
      ; dashboard_arn =
          Aws.Util.option_bind (Aws.Xml.member "DashboardArn" xml) String.parse
      ; last_modified =
          Aws.Util.option_bind (Aws.Xml.member "LastModified" xml) DateTime.parse
      ; size = Aws.Util.option_bind (Aws.Xml.member "Size" xml) Long.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.size (fun f -> Aws.Query.Pair ("Size", Long.to_query f))
         ; Aws.Util.option_map v.last_modified (fun f ->
               Aws.Query.Pair ("LastModified", DateTime.to_query f))
         ; Aws.Util.option_map v.dashboard_arn (fun f ->
               Aws.Query.Pair ("DashboardArn", String.to_query f))
         ; Aws.Util.option_map v.dashboard_name (fun f ->
               Aws.Query.Pair ("DashboardName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.size (fun f -> "Size", Long.to_json f)
         ; Aws.Util.option_map v.last_modified (fun f ->
               "LastModified", DateTime.to_json f)
         ; Aws.Util.option_map v.dashboard_arn (fun f -> "DashboardArn", String.to_json f)
         ; Aws.Util.option_map v.dashboard_name (fun f ->
               "DashboardName", String.to_json f)
         ])

  let of_json j =
    { dashboard_name =
        Aws.Util.option_map (Aws.Json.lookup j "DashboardName") String.of_json
    ; dashboard_arn =
        Aws.Util.option_map (Aws.Json.lookup j "DashboardArn") String.of_json
    ; last_modified =
        Aws.Util.option_map (Aws.Json.lookup j "LastModified") DateTime.of_json
    ; size = Aws.Util.option_map (Aws.Json.lookup j "Size") Long.of_json
    }
end

module DashboardEntries = struct
  type t = DashboardEntry.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map DashboardEntry.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list DashboardEntry.to_query v

  let to_json v = `List (List.map DashboardEntry.to_json v)

  let of_json j = Aws.Json.to_list DashboardEntry.of_json j
end

module ListDashboardsOutput = struct
  type t =
    { dashboard_entries : DashboardEntries.t
    ; next_token : String.t option
    }

  let make ?(dashboard_entries = []) ?next_token () = { dashboard_entries; next_token }

  let parse xml =
    Some
      { dashboard_entries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "DashboardEntries" xml)
               DashboardEntries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("DashboardEntries.member", DashboardEntries.to_query v.dashboard_entries))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("DashboardEntries", DashboardEntries.to_json v.dashboard_entries)
         ])

  let of_json j =
    { dashboard_entries =
        DashboardEntries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardEntries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module InvalidNextToken = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module MetricDataResults = struct
  type t = MetricDataResult.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map MetricDataResult.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list MetricDataResult.to_query v

  let to_json v = `List (List.map MetricDataResult.to_json v)

  let of_json j = Aws.Json.to_list MetricDataResult.of_json j
end

module GetMetricDataOutput = struct
  type t =
    { metric_data_results : MetricDataResults.t
    ; next_token : String.t option
    ; messages : MetricDataResultMessages.t
    }

  let make ?(metric_data_results = []) ?next_token ?(messages = []) () =
    { metric_data_results; next_token; messages }

  let parse xml =
    Some
      { metric_data_results =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "MetricDataResults" xml)
               MetricDataResults.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; messages =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Messages" xml)
               MetricDataResultMessages.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("Messages.member", MetricDataResultMessages.to_query v.messages))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "MetricDataResults.member"
                , MetricDataResults.to_query v.metric_data_results ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Messages", MetricDataResultMessages.to_json v.messages)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("MetricDataResults", MetricDataResults.to_json v.metric_data_results)
         ])

  let of_json j =
    { metric_data_results =
        MetricDataResults.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricDataResults"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; messages =
        MetricDataResultMessages.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Messages"))
    }
end

module InsightRuleNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module PutInsightRuleInput = struct
  type t =
    { rule_name : String.t
    ; rule_state : String.t option
    ; rule_definition : String.t
    ; tags : TagList.t
    }

  let make ~rule_name ?rule_state ~rule_definition ?(tags = []) () =
    { rule_name; rule_state; rule_definition; tags }

  let parse xml =
    Some
      { rule_name =
          Aws.Xml.required
            "RuleName"
            (Aws.Util.option_bind (Aws.Xml.member "RuleName" xml) String.parse)
      ; rule_state = Aws.Util.option_bind (Aws.Xml.member "RuleState" xml) String.parse
      ; rule_definition =
          Aws.Xml.required
            "RuleDefinition"
            (Aws.Util.option_bind (Aws.Xml.member "RuleDefinition" xml) String.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Aws.Query.Pair ("RuleDefinition", String.to_query v.rule_definition))
         ; Aws.Util.option_map v.rule_state (fun f ->
               Aws.Query.Pair ("RuleState", String.to_query f))
         ; Some (Aws.Query.Pair ("RuleName", String.to_query v.rule_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Some ("RuleDefinition", String.to_json v.rule_definition)
         ; Aws.Util.option_map v.rule_state (fun f -> "RuleState", String.to_json f)
         ; Some ("RuleName", String.to_json v.rule_name)
         ])

  let of_json j =
    { rule_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleName"))
    ; rule_state = Aws.Util.option_map (Aws.Json.lookup j "RuleState") String.of_json
    ; rule_definition =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleDefinition"))
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module DescribeInsightRulesInput = struct
  type t =
    { next_token : String.t option
    ; max_results : Integer.t option
    }

  let make ?next_token ?max_results () = { next_token; max_results }

  let parse xml =
    Some
      { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ])

  let of_json j =
    { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    }
end

module CompositeAlarm = struct
  type t =
    { actions_enabled : Boolean.t option
    ; alarm_actions : ResourceList.t
    ; alarm_arn : String.t option
    ; alarm_configuration_updated_timestamp : DateTime.t option
    ; alarm_description : String.t option
    ; alarm_name : String.t option
    ; alarm_rule : String.t option
    ; insufficient_data_actions : ResourceList.t
    ; o_k_actions : ResourceList.t
    ; state_reason : String.t option
    ; state_reason_data : String.t option
    ; state_updated_timestamp : DateTime.t option
    ; state_value : StateValue.t option
    }

  let make
      ?actions_enabled
      ?(alarm_actions = [])
      ?alarm_arn
      ?alarm_configuration_updated_timestamp
      ?alarm_description
      ?alarm_name
      ?alarm_rule
      ?(insufficient_data_actions = [])
      ?(o_k_actions = [])
      ?state_reason
      ?state_reason_data
      ?state_updated_timestamp
      ?state_value
      () =
    { actions_enabled
    ; alarm_actions
    ; alarm_arn
    ; alarm_configuration_updated_timestamp
    ; alarm_description
    ; alarm_name
    ; alarm_rule
    ; insufficient_data_actions
    ; o_k_actions
    ; state_reason
    ; state_reason_data
    ; state_updated_timestamp
    ; state_value
    }

  let parse xml =
    Some
      { actions_enabled =
          Aws.Util.option_bind (Aws.Xml.member "ActionsEnabled" xml) Boolean.parse
      ; alarm_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "AlarmActions" xml) ResourceList.parse)
      ; alarm_arn = Aws.Util.option_bind (Aws.Xml.member "AlarmArn" xml) String.parse
      ; alarm_configuration_updated_timestamp =
          Aws.Util.option_bind
            (Aws.Xml.member "AlarmConfigurationUpdatedTimestamp" xml)
            DateTime.parse
      ; alarm_description =
          Aws.Util.option_bind (Aws.Xml.member "AlarmDescription" xml) String.parse
      ; alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
      ; alarm_rule = Aws.Util.option_bind (Aws.Xml.member "AlarmRule" xml) String.parse
      ; insufficient_data_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "InsufficientDataActions" xml)
               ResourceList.parse)
      ; o_k_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "OKActions" xml) ResourceList.parse)
      ; state_reason =
          Aws.Util.option_bind (Aws.Xml.member "StateReason" xml) String.parse
      ; state_reason_data =
          Aws.Util.option_bind (Aws.Xml.member "StateReasonData" xml) String.parse
      ; state_updated_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "StateUpdatedTimestamp" xml) DateTime.parse
      ; state_value =
          Aws.Util.option_bind (Aws.Xml.member "StateValue" xml) StateValue.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.state_value (fun f ->
               Aws.Query.Pair ("StateValue", StateValue.to_query f))
         ; Aws.Util.option_map v.state_updated_timestamp (fun f ->
               Aws.Query.Pair ("StateUpdatedTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.state_reason_data (fun f ->
               Aws.Query.Pair ("StateReasonData", String.to_query f))
         ; Aws.Util.option_map v.state_reason (fun f ->
               Aws.Query.Pair ("StateReason", String.to_query f))
         ; Some (Aws.Query.Pair ("OKActions.member", ResourceList.to_query v.o_k_actions))
         ; Some
             (Aws.Query.Pair
                ( "InsufficientDataActions.member"
                , ResourceList.to_query v.insufficient_data_actions ))
         ; Aws.Util.option_map v.alarm_rule (fun f ->
               Aws.Query.Pair ("AlarmRule", String.to_query f))
         ; Aws.Util.option_map v.alarm_name (fun f ->
               Aws.Query.Pair ("AlarmName", String.to_query f))
         ; Aws.Util.option_map v.alarm_description (fun f ->
               Aws.Query.Pair ("AlarmDescription", String.to_query f))
         ; Aws.Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
               Aws.Query.Pair ("AlarmConfigurationUpdatedTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.alarm_arn (fun f ->
               Aws.Query.Pair ("AlarmArn", String.to_query f))
         ; Some
             (Aws.Query.Pair ("AlarmActions.member", ResourceList.to_query v.alarm_actions))
         ; Aws.Util.option_map v.actions_enabled (fun f ->
               Aws.Query.Pair ("ActionsEnabled", Boolean.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.state_value (fun f -> "StateValue", StateValue.to_json f)
         ; Aws.Util.option_map v.state_updated_timestamp (fun f ->
               "StateUpdatedTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.state_reason_data (fun f ->
               "StateReasonData", String.to_json f)
         ; Aws.Util.option_map v.state_reason (fun f -> "StateReason", String.to_json f)
         ; Some ("OKActions", ResourceList.to_json v.o_k_actions)
         ; Some
             ("InsufficientDataActions", ResourceList.to_json v.insufficient_data_actions)
         ; Aws.Util.option_map v.alarm_rule (fun f -> "AlarmRule", String.to_json f)
         ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
         ; Aws.Util.option_map v.alarm_description (fun f ->
               "AlarmDescription", String.to_json f)
         ; Aws.Util.option_map v.alarm_configuration_updated_timestamp (fun f ->
               "AlarmConfigurationUpdatedTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.alarm_arn (fun f -> "AlarmArn", String.to_json f)
         ; Some ("AlarmActions", ResourceList.to_json v.alarm_actions)
         ; Aws.Util.option_map v.actions_enabled (fun f ->
               "ActionsEnabled", Boolean.to_json f)
         ])

  let of_json j =
    { actions_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "ActionsEnabled") Boolean.of_json
    ; alarm_actions =
        ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmActions"))
    ; alarm_arn = Aws.Util.option_map (Aws.Json.lookup j "AlarmArn") String.of_json
    ; alarm_configuration_updated_timestamp =
        Aws.Util.option_map
          (Aws.Json.lookup j "AlarmConfigurationUpdatedTimestamp")
          DateTime.of_json
    ; alarm_description =
        Aws.Util.option_map (Aws.Json.lookup j "AlarmDescription") String.of_json
    ; alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
    ; alarm_rule = Aws.Util.option_map (Aws.Json.lookup j "AlarmRule") String.of_json
    ; insufficient_data_actions =
        ResourceList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "InsufficientDataActions"))
    ; o_k_actions =
        ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OKActions"))
    ; state_reason = Aws.Util.option_map (Aws.Json.lookup j "StateReason") String.of_json
    ; state_reason_data =
        Aws.Util.option_map (Aws.Json.lookup j "StateReasonData") String.of_json
    ; state_updated_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "StateUpdatedTimestamp") DateTime.of_json
    ; state_value =
        Aws.Util.option_map (Aws.Json.lookup j "StateValue") StateValue.of_json
    }
end

module CompositeAlarms = struct
  type t = CompositeAlarm.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map CompositeAlarm.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list CompositeAlarm.to_query v

  let to_json v = `List (List.map CompositeAlarm.to_json v)

  let of_json j = Aws.Json.to_list CompositeAlarm.of_json j
end

module DescribeAlarmsOutput = struct
  type t =
    { composite_alarms : CompositeAlarms.t
    ; metric_alarms : MetricAlarms.t
    ; next_token : String.t option
    }

  let make ?(composite_alarms = []) ?(metric_alarms = []) ?next_token () =
    { composite_alarms; metric_alarms; next_token }

  let parse xml =
    Some
      { composite_alarms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CompositeAlarms" xml)
               CompositeAlarms.parse)
      ; metric_alarms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "MetricAlarms" xml) MetricAlarms.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair ("MetricAlarms.member", MetricAlarms.to_query v.metric_alarms))
         ; Some
             (Aws.Query.Pair
                ("CompositeAlarms.member", CompositeAlarms.to_query v.composite_alarms))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("MetricAlarms", MetricAlarms.to_json v.metric_alarms)
         ; Some ("CompositeAlarms", CompositeAlarms.to_json v.composite_alarms)
         ])

  let of_json j =
    { composite_alarms =
        CompositeAlarms.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CompositeAlarms"))
    ; metric_alarms =
        MetricAlarms.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricAlarms"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DeleteDashboardsOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ResourceNotFound = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module PutDashboardInput = struct
  type t =
    { dashboard_name : String.t
    ; dashboard_body : String.t
    }

  let make ~dashboard_name ~dashboard_body () = { dashboard_name; dashboard_body }

  let parse xml =
    Some
      { dashboard_name =
          Aws.Xml.required
            "DashboardName"
            (Aws.Util.option_bind (Aws.Xml.member "DashboardName" xml) String.parse)
      ; dashboard_body =
          Aws.Xml.required
            "DashboardBody"
            (Aws.Util.option_bind (Aws.Xml.member "DashboardBody" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("DashboardBody", String.to_query v.dashboard_body))
         ; Some (Aws.Query.Pair ("DashboardName", String.to_query v.dashboard_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("DashboardBody", String.to_json v.dashboard_body)
         ; Some ("DashboardName", String.to_json v.dashboard_name)
         ])

  let of_json j =
    { dashboard_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardName"))
    ; dashboard_body =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardBody"))
    }
end

module PutAnomalyDetectorOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeAnomalyDetectorsOutput = struct
  type t =
    { anomaly_detectors : AnomalyDetectors.t
    ; next_token : String.t option
    }

  let make ?(anomaly_detectors = []) ?next_token () = { anomaly_detectors; next_token }

  let parse xml =
    Some
      { anomaly_detectors =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AnomalyDetectors" xml)
               AnomalyDetectors.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("AnomalyDetectors.member", AnomalyDetectors.to_query v.anomaly_detectors))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("AnomalyDetectors", AnomalyDetectors.to_json v.anomaly_detectors)
         ])

  let of_json j =
    { anomaly_detectors =
        AnomalyDetectors.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AnomalyDetectors"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module InsightRuleMetricList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module InvalidParameterValueException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DeleteInsightRulesInput = struct
  type t = { rule_names : InsightRuleNames.t }

  let make ~rule_names () = { rule_names }

  let parse xml =
    Some
      { rule_names =
          Aws.Xml.required
            "RuleNames"
            (Aws.Util.option_bind (Aws.Xml.member "RuleNames" xml) InsightRuleNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("RuleNames.member", InsightRuleNames.to_query v.rule_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("RuleNames", InsightRuleNames.to_json v.rule_names) ])

  let of_json j =
    { rule_names =
        InsightRuleNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleNames"))
    }
end

module DashboardInvalidInputError = struct
  type t =
    { message : String.t option
    ; dashboard_validation_messages : DashboardValidationMessages.t
    }

  let make ?message ?(dashboard_validation_messages = []) () =
    { message; dashboard_validation_messages }

  let parse xml =
    Some
      { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse
      ; dashboard_validation_messages =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "dashboardValidationMessages" xml)
               DashboardValidationMessages.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "dashboardValidationMessages.member"
                , DashboardValidationMessages.to_query v.dashboard_validation_messages ))
         ; Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "dashboardValidationMessages"
             , DashboardValidationMessages.to_json v.dashboard_validation_messages )
         ; Aws.Util.option_map v.message (fun f -> "message", String.to_json f)
         ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json
    ; dashboard_validation_messages =
        DashboardValidationMessages.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "dashboardValidationMessages"))
    }
end

module TagKeyList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module InsightRuleMetricDatapoint = struct
  type t =
    { timestamp : DateTime.t
    ; unique_contributors : Double.t option
    ; max_contributor_value : Double.t option
    ; sample_count : Double.t option
    ; average : Double.t option
    ; sum : Double.t option
    ; minimum : Double.t option
    ; maximum : Double.t option
    }

  let make
      ~timestamp
      ?unique_contributors
      ?max_contributor_value
      ?sample_count
      ?average
      ?sum
      ?minimum
      ?maximum
      () =
    { timestamp
    ; unique_contributors
    ; max_contributor_value
    ; sample_count
    ; average
    ; sum
    ; minimum
    ; maximum
    }

  let parse xml =
    Some
      { timestamp =
          Aws.Xml.required
            "Timestamp"
            (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
      ; unique_contributors =
          Aws.Util.option_bind (Aws.Xml.member "UniqueContributors" xml) Double.parse
      ; max_contributor_value =
          Aws.Util.option_bind (Aws.Xml.member "MaxContributorValue" xml) Double.parse
      ; sample_count =
          Aws.Util.option_bind (Aws.Xml.member "SampleCount" xml) Double.parse
      ; average = Aws.Util.option_bind (Aws.Xml.member "Average" xml) Double.parse
      ; sum = Aws.Util.option_bind (Aws.Xml.member "Sum" xml) Double.parse
      ; minimum = Aws.Util.option_bind (Aws.Xml.member "Minimum" xml) Double.parse
      ; maximum = Aws.Util.option_bind (Aws.Xml.member "Maximum" xml) Double.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.maximum (fun f ->
               Aws.Query.Pair ("Maximum", Double.to_query f))
         ; Aws.Util.option_map v.minimum (fun f ->
               Aws.Query.Pair ("Minimum", Double.to_query f))
         ; Aws.Util.option_map v.sum (fun f -> Aws.Query.Pair ("Sum", Double.to_query f))
         ; Aws.Util.option_map v.average (fun f ->
               Aws.Query.Pair ("Average", Double.to_query f))
         ; Aws.Util.option_map v.sample_count (fun f ->
               Aws.Query.Pair ("SampleCount", Double.to_query f))
         ; Aws.Util.option_map v.max_contributor_value (fun f ->
               Aws.Query.Pair ("MaxContributorValue", Double.to_query f))
         ; Aws.Util.option_map v.unique_contributors (fun f ->
               Aws.Query.Pair ("UniqueContributors", Double.to_query f))
         ; Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.maximum (fun f -> "Maximum", Double.to_json f)
         ; Aws.Util.option_map v.minimum (fun f -> "Minimum", Double.to_json f)
         ; Aws.Util.option_map v.sum (fun f -> "Sum", Double.to_json f)
         ; Aws.Util.option_map v.average (fun f -> "Average", Double.to_json f)
         ; Aws.Util.option_map v.sample_count (fun f -> "SampleCount", Double.to_json f)
         ; Aws.Util.option_map v.max_contributor_value (fun f ->
               "MaxContributorValue", Double.to_json f)
         ; Aws.Util.option_map v.unique_contributors (fun f ->
               "UniqueContributors", Double.to_json f)
         ; Some ("Timestamp", DateTime.to_json v.timestamp)
         ])

  let of_json j =
    { timestamp =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
    ; unique_contributors =
        Aws.Util.option_map (Aws.Json.lookup j "UniqueContributors") Double.of_json
    ; max_contributor_value =
        Aws.Util.option_map (Aws.Json.lookup j "MaxContributorValue") Double.of_json
    ; sample_count = Aws.Util.option_map (Aws.Json.lookup j "SampleCount") Double.of_json
    ; average = Aws.Util.option_map (Aws.Json.lookup j "Average") Double.of_json
    ; sum = Aws.Util.option_map (Aws.Json.lookup j "Sum") Double.of_json
    ; minimum = Aws.Util.option_map (Aws.Json.lookup j "Minimum") Double.of_json
    ; maximum = Aws.Util.option_map (Aws.Json.lookup j "Maximum") Double.of_json
    }
end

module InsightRuleMetricDatapoints = struct
  type t = InsightRuleMetricDatapoint.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map InsightRuleMetricDatapoint.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list InsightRuleMetricDatapoint.to_query v

  let to_json v = `List (List.map InsightRuleMetricDatapoint.to_json v)

  let of_json j = Aws.Json.to_list InsightRuleMetricDatapoint.of_json j
end

module InsightRuleContributors = struct
  type t = InsightRuleContributor.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map InsightRuleContributor.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list InsightRuleContributor.to_query v

  let to_json v = `List (List.map InsightRuleContributor.to_json v)

  let of_json j = Aws.Json.to_list InsightRuleContributor.of_json j
end

module InsightRuleContributorKeyLabels = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module GetInsightRuleReportOutput = struct
  type t =
    { key_labels : InsightRuleContributorKeyLabels.t
    ; aggregation_statistic : String.t option
    ; aggregate_value : Double.t option
    ; approximate_unique_count : Long.t option
    ; contributors : InsightRuleContributors.t
    ; metric_datapoints : InsightRuleMetricDatapoints.t
    }

  let make
      ?(key_labels = [])
      ?aggregation_statistic
      ?aggregate_value
      ?approximate_unique_count
      ?(contributors = [])
      ?(metric_datapoints = [])
      () =
    { key_labels
    ; aggregation_statistic
    ; aggregate_value
    ; approximate_unique_count
    ; contributors
    ; metric_datapoints
    }

  let parse xml =
    Some
      { key_labels =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "KeyLabels" xml)
               InsightRuleContributorKeyLabels.parse)
      ; aggregation_statistic =
          Aws.Util.option_bind (Aws.Xml.member "AggregationStatistic" xml) String.parse
      ; aggregate_value =
          Aws.Util.option_bind (Aws.Xml.member "AggregateValue" xml) Double.parse
      ; approximate_unique_count =
          Aws.Util.option_bind (Aws.Xml.member "ApproximateUniqueCount" xml) Long.parse
      ; contributors =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Contributors" xml)
               InsightRuleContributors.parse)
      ; metric_datapoints =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "MetricDatapoints" xml)
               InsightRuleMetricDatapoints.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "MetricDatapoints.member"
                , InsightRuleMetricDatapoints.to_query v.metric_datapoints ))
         ; Some
             (Aws.Query.Pair
                ("Contributors.member", InsightRuleContributors.to_query v.contributors))
         ; Aws.Util.option_map v.approximate_unique_count (fun f ->
               Aws.Query.Pair ("ApproximateUniqueCount", Long.to_query f))
         ; Aws.Util.option_map v.aggregate_value (fun f ->
               Aws.Query.Pair ("AggregateValue", Double.to_query f))
         ; Aws.Util.option_map v.aggregation_statistic (fun f ->
               Aws.Query.Pair ("AggregationStatistic", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("KeyLabels.member", InsightRuleContributorKeyLabels.to_query v.key_labels))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("MetricDatapoints", InsightRuleMetricDatapoints.to_json v.metric_datapoints)
         ; Some ("Contributors", InsightRuleContributors.to_json v.contributors)
         ; Aws.Util.option_map v.approximate_unique_count (fun f ->
               "ApproximateUniqueCount", Long.to_json f)
         ; Aws.Util.option_map v.aggregate_value (fun f ->
               "AggregateValue", Double.to_json f)
         ; Aws.Util.option_map v.aggregation_statistic (fun f ->
               "AggregationStatistic", String.to_json f)
         ; Some ("KeyLabels", InsightRuleContributorKeyLabels.to_json v.key_labels)
         ])

  let of_json j =
    { key_labels =
        InsightRuleContributorKeyLabels.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyLabels"))
    ; aggregation_statistic =
        Aws.Util.option_map (Aws.Json.lookup j "AggregationStatistic") String.of_json
    ; aggregate_value =
        Aws.Util.option_map (Aws.Json.lookup j "AggregateValue") Double.of_json
    ; approximate_unique_count =
        Aws.Util.option_map (Aws.Json.lookup j "ApproximateUniqueCount") Long.of_json
    ; contributors =
        InsightRuleContributors.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Contributors"))
    ; metric_datapoints =
        InsightRuleMetricDatapoints.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricDatapoints"))
    }
end

module DescribeAlarmsForMetricInput = struct
  type t =
    { metric_name : String.t
    ; namespace : String.t
    ; statistic : Statistic.t option
    ; extended_statistic : String.t option
    ; dimensions : Dimensions.t
    ; period : Integer.t option
    ; unit : StandardUnit.t option
    }

  let make
      ~metric_name
      ~namespace
      ?statistic
      ?extended_statistic
      ?(dimensions = [])
      ?period
      ?unit
      () =
    { metric_name; namespace; statistic; extended_statistic; dimensions; period; unit }

  let parse xml =
    Some
      { metric_name =
          Aws.Xml.required
            "MetricName"
            (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
      ; namespace =
          Aws.Xml.required
            "Namespace"
            (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
      ; statistic = Aws.Util.option_bind (Aws.Xml.member "Statistic" xml) Statistic.parse
      ; extended_statistic =
          Aws.Util.option_bind (Aws.Xml.member "ExtendedStatistic" xml) String.parse
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
      ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.unit (fun f ->
               Aws.Query.Pair ("Unit", StandardUnit.to_query f))
         ; Aws.Util.option_map v.period (fun f ->
               Aws.Query.Pair ("Period", Integer.to_query f))
         ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Aws.Util.option_map v.extended_statistic (fun f ->
               Aws.Query.Pair ("ExtendedStatistic", String.to_query f))
         ; Aws.Util.option_map v.statistic (fun f ->
               Aws.Query.Pair ("Statistic", Statistic.to_query f))
         ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
         ; Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
         ; Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
         ; Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Aws.Util.option_map v.extended_statistic (fun f ->
               "ExtendedStatistic", String.to_json f)
         ; Aws.Util.option_map v.statistic (fun f -> "Statistic", Statistic.to_json f)
         ; Some ("Namespace", String.to_json v.namespace)
         ; Some ("MetricName", String.to_json v.metric_name)
         ])

  let of_json j =
    { metric_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
    ; namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
    ; statistic = Aws.Util.option_map (Aws.Json.lookup j "Statistic") Statistic.of_json
    ; extended_statistic =
        Aws.Util.option_map (Aws.Json.lookup j "ExtendedStatistic") String.of_json
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
    ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
    }
end

module GetMetricWidgetImageOutput = struct
  type t = { metric_widget_image : Blob.t option }

  let make ?metric_widget_image () = { metric_widget_image }

  let parse xml =
    Some
      { metric_widget_image =
          Aws.Util.option_bind (Aws.Xml.member "MetricWidgetImage" xml) Blob.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.metric_widget_image (fun f ->
               Aws.Query.Pair ("MetricWidgetImage", Blob.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.metric_widget_image (fun f ->
               "MetricWidgetImage", Blob.to_json f)
         ])

  let of_json j =
    { metric_widget_image =
        Aws.Util.option_map (Aws.Json.lookup j "MetricWidgetImage") Blob.of_json
    }
end

module ListDashboardsInput = struct
  type t =
    { dashboard_name_prefix : String.t option
    ; next_token : String.t option
    }

  let make ?dashboard_name_prefix ?next_token () = { dashboard_name_prefix; next_token }

  let parse xml =
    Some
      { dashboard_name_prefix =
          Aws.Util.option_bind (Aws.Xml.member "DashboardNamePrefix" xml) String.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.dashboard_name_prefix (fun f ->
               Aws.Query.Pair ("DashboardNamePrefix", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.dashboard_name_prefix (fun f ->
               "DashboardNamePrefix", String.to_json f)
         ])

  let of_json j =
    { dashboard_name_prefix =
        Aws.Util.option_map (Aws.Json.lookup j "DashboardNamePrefix") String.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module EnableAlarmActionsInput = struct
  type t = { alarm_names : AlarmNames.t }

  let make ~alarm_names () = { alarm_names }

  let parse xml =
    Some
      { alarm_names =
          Aws.Xml.required
            "AlarmNames"
            (Aws.Util.option_bind (Aws.Xml.member "AlarmNames" xml) AlarmNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("AlarmNames", AlarmNames.to_json v.alarm_names) ])

  let of_json j =
    { alarm_names =
        AlarmNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmNames"))
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
          Aws.Xml.required
            "AlarmName"
            (Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse)
      ; state_value =
          Aws.Xml.required
            "StateValue"
            (Aws.Util.option_bind (Aws.Xml.member "StateValue" xml) StateValue.parse)
      ; state_reason =
          Aws.Xml.required
            "StateReason"
            (Aws.Util.option_bind (Aws.Xml.member "StateReason" xml) String.parse)
      ; state_reason_data =
          Aws.Util.option_bind (Aws.Xml.member "StateReasonData" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.state_reason_data (fun f ->
               Aws.Query.Pair ("StateReasonData", String.to_query f))
         ; Some (Aws.Query.Pair ("StateReason", String.to_query v.state_reason))
         ; Some (Aws.Query.Pair ("StateValue", StateValue.to_query v.state_value))
         ; Some (Aws.Query.Pair ("AlarmName", String.to_query v.alarm_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.state_reason_data (fun f ->
               "StateReasonData", String.to_json f)
         ; Some ("StateReason", String.to_json v.state_reason)
         ; Some ("StateValue", StateValue.to_json v.state_value)
         ; Some ("AlarmName", String.to_json v.alarm_name)
         ])

  let of_json j =
    { alarm_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmName"))
    ; state_value =
        StateValue.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StateValue"))
    ; state_reason =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StateReason"))
    ; state_reason_data =
        Aws.Util.option_map (Aws.Json.lookup j "StateReasonData") String.of_json
    }
end

module PutMetricAlarmInput = struct
  type t =
    { alarm_name : String.t
    ; alarm_description : String.t option
    ; actions_enabled : Boolean.t option
    ; o_k_actions : ResourceList.t
    ; alarm_actions : ResourceList.t
    ; insufficient_data_actions : ResourceList.t
    ; metric_name : String.t option
    ; namespace : String.t option
    ; statistic : Statistic.t option
    ; extended_statistic : String.t option
    ; dimensions : Dimensions.t
    ; period : Integer.t option
    ; unit : StandardUnit.t option
    ; evaluation_periods : Integer.t
    ; datapoints_to_alarm : Integer.t option
    ; threshold : Double.t option
    ; comparison_operator : ComparisonOperator.t
    ; treat_missing_data : String.t option
    ; evaluate_low_sample_count_percentile : String.t option
    ; metrics : MetricDataQueries.t
    ; tags : TagList.t
    ; threshold_metric_id : String.t option
    }

  let make
      ~alarm_name
      ?alarm_description
      ?actions_enabled
      ?(o_k_actions = [])
      ?(alarm_actions = [])
      ?(insufficient_data_actions = [])
      ?metric_name
      ?namespace
      ?statistic
      ?extended_statistic
      ?(dimensions = [])
      ?period
      ?unit
      ~evaluation_periods
      ?datapoints_to_alarm
      ?threshold
      ~comparison_operator
      ?treat_missing_data
      ?evaluate_low_sample_count_percentile
      ?(metrics = [])
      ?(tags = [])
      ?threshold_metric_id
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
    ; extended_statistic
    ; dimensions
    ; period
    ; unit
    ; evaluation_periods
    ; datapoints_to_alarm
    ; threshold
    ; comparison_operator
    ; treat_missing_data
    ; evaluate_low_sample_count_percentile
    ; metrics
    ; tags
    ; threshold_metric_id
    }

  let parse xml =
    Some
      { alarm_name =
          Aws.Xml.required
            "AlarmName"
            (Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse)
      ; alarm_description =
          Aws.Util.option_bind (Aws.Xml.member "AlarmDescription" xml) String.parse
      ; actions_enabled =
          Aws.Util.option_bind (Aws.Xml.member "ActionsEnabled" xml) Boolean.parse
      ; o_k_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "OKActions" xml) ResourceList.parse)
      ; alarm_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "AlarmActions" xml) ResourceList.parse)
      ; insufficient_data_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "InsufficientDataActions" xml)
               ResourceList.parse)
      ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
      ; namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
      ; statistic = Aws.Util.option_bind (Aws.Xml.member "Statistic" xml) Statistic.parse
      ; extended_statistic =
          Aws.Util.option_bind (Aws.Xml.member "ExtendedStatistic" xml) String.parse
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) Dimensions.parse)
      ; period = Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse
      ; unit = Aws.Util.option_bind (Aws.Xml.member "Unit" xml) StandardUnit.parse
      ; evaluation_periods =
          Aws.Xml.required
            "EvaluationPeriods"
            (Aws.Util.option_bind (Aws.Xml.member "EvaluationPeriods" xml) Integer.parse)
      ; datapoints_to_alarm =
          Aws.Util.option_bind (Aws.Xml.member "DatapointsToAlarm" xml) Integer.parse
      ; threshold = Aws.Util.option_bind (Aws.Xml.member "Threshold" xml) Double.parse
      ; comparison_operator =
          Aws.Xml.required
            "ComparisonOperator"
            (Aws.Util.option_bind
               (Aws.Xml.member "ComparisonOperator" xml)
               ComparisonOperator.parse)
      ; treat_missing_data =
          Aws.Util.option_bind (Aws.Xml.member "TreatMissingData" xml) String.parse
      ; evaluate_low_sample_count_percentile =
          Aws.Util.option_bind
            (Aws.Xml.member "EvaluateLowSampleCountPercentile" xml)
            String.parse
      ; metrics =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml) MetricDataQueries.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      ; threshold_metric_id =
          Aws.Util.option_bind (Aws.Xml.member "ThresholdMetricId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.threshold_metric_id (fun f ->
               Aws.Query.Pair ("ThresholdMetricId", String.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Aws.Query.Pair ("Metrics.member", MetricDataQueries.to_query v.metrics))
         ; Aws.Util.option_map v.evaluate_low_sample_count_percentile (fun f ->
               Aws.Query.Pair ("EvaluateLowSampleCountPercentile", String.to_query f))
         ; Aws.Util.option_map v.treat_missing_data (fun f ->
               Aws.Query.Pair ("TreatMissingData", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ComparisonOperator", ComparisonOperator.to_query v.comparison_operator))
         ; Aws.Util.option_map v.threshold (fun f ->
               Aws.Query.Pair ("Threshold", Double.to_query f))
         ; Aws.Util.option_map v.datapoints_to_alarm (fun f ->
               Aws.Query.Pair ("DatapointsToAlarm", Integer.to_query f))
         ; Some
             (Aws.Query.Pair ("EvaluationPeriods", Integer.to_query v.evaluation_periods))
         ; Aws.Util.option_map v.unit (fun f ->
               Aws.Query.Pair ("Unit", StandardUnit.to_query f))
         ; Aws.Util.option_map v.period (fun f ->
               Aws.Query.Pair ("Period", Integer.to_query f))
         ; Some (Aws.Query.Pair ("Dimensions.member", Dimensions.to_query v.dimensions))
         ; Aws.Util.option_map v.extended_statistic (fun f ->
               Aws.Query.Pair ("ExtendedStatistic", String.to_query f))
         ; Aws.Util.option_map v.statistic (fun f ->
               Aws.Query.Pair ("Statistic", Statistic.to_query f))
         ; Aws.Util.option_map v.namespace (fun f ->
               Aws.Query.Pair ("Namespace", String.to_query f))
         ; Aws.Util.option_map v.metric_name (fun f ->
               Aws.Query.Pair ("MetricName", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "InsufficientDataActions.member"
                , ResourceList.to_query v.insufficient_data_actions ))
         ; Some
             (Aws.Query.Pair ("AlarmActions.member", ResourceList.to_query v.alarm_actions))
         ; Some (Aws.Query.Pair ("OKActions.member", ResourceList.to_query v.o_k_actions))
         ; Aws.Util.option_map v.actions_enabled (fun f ->
               Aws.Query.Pair ("ActionsEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.alarm_description (fun f ->
               Aws.Query.Pair ("AlarmDescription", String.to_query f))
         ; Some (Aws.Query.Pair ("AlarmName", String.to_query v.alarm_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.threshold_metric_id (fun f ->
               "ThresholdMetricId", String.to_json f)
         ; Some ("Tags", TagList.to_json v.tags)
         ; Some ("Metrics", MetricDataQueries.to_json v.metrics)
         ; Aws.Util.option_map v.evaluate_low_sample_count_percentile (fun f ->
               "EvaluateLowSampleCountPercentile", String.to_json f)
         ; Aws.Util.option_map v.treat_missing_data (fun f ->
               "TreatMissingData", String.to_json f)
         ; Some ("ComparisonOperator", ComparisonOperator.to_json v.comparison_operator)
         ; Aws.Util.option_map v.threshold (fun f -> "Threshold", Double.to_json f)
         ; Aws.Util.option_map v.datapoints_to_alarm (fun f ->
               "DatapointsToAlarm", Integer.to_json f)
         ; Some ("EvaluationPeriods", Integer.to_json v.evaluation_periods)
         ; Aws.Util.option_map v.unit (fun f -> "Unit", StandardUnit.to_json f)
         ; Aws.Util.option_map v.period (fun f -> "Period", Integer.to_json f)
         ; Some ("Dimensions", Dimensions.to_json v.dimensions)
         ; Aws.Util.option_map v.extended_statistic (fun f ->
               "ExtendedStatistic", String.to_json f)
         ; Aws.Util.option_map v.statistic (fun f -> "Statistic", Statistic.to_json f)
         ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
         ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
         ; Some
             ("InsufficientDataActions", ResourceList.to_json v.insufficient_data_actions)
         ; Some ("AlarmActions", ResourceList.to_json v.alarm_actions)
         ; Some ("OKActions", ResourceList.to_json v.o_k_actions)
         ; Aws.Util.option_map v.actions_enabled (fun f ->
               "ActionsEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.alarm_description (fun f ->
               "AlarmDescription", String.to_json f)
         ; Some ("AlarmName", String.to_json v.alarm_name)
         ])

  let of_json j =
    { alarm_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmName"))
    ; alarm_description =
        Aws.Util.option_map (Aws.Json.lookup j "AlarmDescription") String.of_json
    ; actions_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "ActionsEnabled") Boolean.of_json
    ; o_k_actions =
        ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OKActions"))
    ; alarm_actions =
        ResourceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmActions"))
    ; insufficient_data_actions =
        ResourceList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "InsufficientDataActions"))
    ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
    ; namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
    ; statistic = Aws.Util.option_map (Aws.Json.lookup j "Statistic") Statistic.of_json
    ; extended_statistic =
        Aws.Util.option_map (Aws.Json.lookup j "ExtendedStatistic") String.of_json
    ; dimensions =
        Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    ; period = Aws.Util.option_map (Aws.Json.lookup j "Period") Integer.of_json
    ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
    ; evaluation_periods =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EvaluationPeriods"))
    ; datapoints_to_alarm =
        Aws.Util.option_map (Aws.Json.lookup j "DatapointsToAlarm") Integer.of_json
    ; threshold = Aws.Util.option_map (Aws.Json.lookup j "Threshold") Double.of_json
    ; comparison_operator =
        ComparisonOperator.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ComparisonOperator"))
    ; treat_missing_data =
        Aws.Util.option_map (Aws.Json.lookup j "TreatMissingData") String.of_json
    ; evaluate_low_sample_count_percentile =
        Aws.Util.option_map
          (Aws.Json.lookup j "EvaluateLowSampleCountPercentile")
          String.of_json
    ; metrics =
        MetricDataQueries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; threshold_metric_id =
        Aws.Util.option_map (Aws.Json.lookup j "ThresholdMetricId") String.of_json
    }
end

module LimitExceededFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module EnableInsightRulesOutput = struct
  type t = { failures : BatchFailures.t }

  let make ?(failures = []) () = { failures }

  let parse xml =
    Some
      { failures =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Failures" xml) BatchFailures.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Failures.member", BatchFailures.to_query v.failures)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Failures", BatchFailures.to_json v.failures) ])

  let of_json j =
    { failures =
        BatchFailures.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Failures"))
    }
end

module GetMetricWidgetImageInput = struct
  type t =
    { metric_widget : String.t
    ; output_format : String.t option
    }

  let make ~metric_widget ?output_format () = { metric_widget; output_format }

  let parse xml =
    Some
      { metric_widget =
          Aws.Xml.required
            "MetricWidget"
            (Aws.Util.option_bind (Aws.Xml.member "MetricWidget" xml) String.parse)
      ; output_format =
          Aws.Util.option_bind (Aws.Xml.member "OutputFormat" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.output_format (fun f ->
               Aws.Query.Pair ("OutputFormat", String.to_query f))
         ; Some (Aws.Query.Pair ("MetricWidget", String.to_query v.metric_widget))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.output_format (fun f -> "OutputFormat", String.to_json f)
         ; Some ("MetricWidget", String.to_json v.metric_widget)
         ])

  let of_json j =
    { metric_widget =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricWidget"))
    ; output_format =
        Aws.Util.option_map (Aws.Json.lookup j "OutputFormat") String.of_json
    }
end

module ScanBy = struct
  type t =
    | TimestampDescending
    | TimestampAscending

  let str_to_t =
    [ "TimestampAscending", TimestampAscending
    ; "TimestampDescending", TimestampDescending
    ]

  let t_to_str =
    [ TimestampAscending, "TimestampAscending"
    ; TimestampDescending, "TimestampDescending"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module UntagResourceOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module GetMetricDataInput = struct
  type t =
    { metric_data_queries : MetricDataQueries.t
    ; start_time : DateTime.t
    ; end_time : DateTime.t
    ; next_token : String.t option
    ; scan_by : ScanBy.t option
    ; max_datapoints : Integer.t option
    }

  let make
      ~metric_data_queries
      ~start_time
      ~end_time
      ?next_token
      ?scan_by
      ?max_datapoints
      () =
    { metric_data_queries; start_time; end_time; next_token; scan_by; max_datapoints }

  let parse xml =
    Some
      { metric_data_queries =
          Aws.Xml.required
            "MetricDataQueries"
            (Aws.Util.option_bind
               (Aws.Xml.member "MetricDataQueries" xml)
               MetricDataQueries.parse)
      ; start_time =
          Aws.Xml.required
            "StartTime"
            (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
      ; end_time =
          Aws.Xml.required
            "EndTime"
            (Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; scan_by = Aws.Util.option_bind (Aws.Xml.member "ScanBy" xml) ScanBy.parse
      ; max_datapoints =
          Aws.Util.option_bind (Aws.Xml.member "MaxDatapoints" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_datapoints (fun f ->
               Aws.Query.Pair ("MaxDatapoints", Integer.to_query f))
         ; Aws.Util.option_map v.scan_by (fun f ->
               Aws.Query.Pair ("ScanBy", ScanBy.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("EndTime", DateTime.to_query v.end_time))
         ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
         ; Some
             (Aws.Query.Pair
                ( "MetricDataQueries.member"
                , MetricDataQueries.to_query v.metric_data_queries ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_datapoints (fun f ->
               "MaxDatapoints", Integer.to_json f)
         ; Aws.Util.option_map v.scan_by (fun f -> "ScanBy", ScanBy.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("EndTime", DateTime.to_json v.end_time)
         ; Some ("StartTime", DateTime.to_json v.start_time)
         ; Some ("MetricDataQueries", MetricDataQueries.to_json v.metric_data_queries)
         ])

  let of_json j =
    { metric_data_queries =
        MetricDataQueries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricDataQueries"))
    ; start_time =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
    ; end_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndTime"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; scan_by = Aws.Util.option_map (Aws.Json.lookup j "ScanBy") ScanBy.of_json
    ; max_datapoints =
        Aws.Util.option_map (Aws.Json.lookup j "MaxDatapoints") Integer.of_json
    }
end

module UntagResourceInput = struct
  type t =
    { resource_a_r_n : String.t
    ; tag_keys : TagKeyList.t
    }

  let make ~resource_a_r_n ~tag_keys () = { resource_a_r_n; tag_keys }

  let parse xml =
    Some
      { resource_a_r_n =
          Aws.Xml.required
            "ResourceARN"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceARN" xml) String.parse)
      ; tag_keys =
          Aws.Xml.required
            "TagKeys"
            (Aws.Util.option_bind (Aws.Xml.member "TagKeys" xml) TagKeyList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagKeys.member", TagKeyList.to_query v.tag_keys))
         ; Some (Aws.Query.Pair ("ResourceARN", String.to_query v.resource_a_r_n))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagKeys", TagKeyList.to_json v.tag_keys)
         ; Some ("ResourceARN", String.to_json v.resource_a_r_n)
         ])

  let of_json j =
    { resource_a_r_n =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceARN"))
    ; tag_keys = TagKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKeys"))
    }
end

module DisableInsightRulesInput = struct
  type t = { rule_names : InsightRuleNames.t }

  let make ~rule_names () = { rule_names }

  let parse xml =
    Some
      { rule_names =
          Aws.Xml.required
            "RuleNames"
            (Aws.Util.option_bind (Aws.Xml.member "RuleNames" xml) InsightRuleNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("RuleNames.member", InsightRuleNames.to_query v.rule_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("RuleNames", InsightRuleNames.to_json v.rule_names) ])

  let of_json j =
    { rule_names =
        InsightRuleNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleNames"))
    }
end

module ListTagsForResourceInput = struct
  type t = { resource_a_r_n : String.t }

  let make ~resource_a_r_n () = { resource_a_r_n }

  let parse xml =
    Some
      { resource_a_r_n =
          Aws.Xml.required
            "ResourceARN"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceARN" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ResourceARN", String.to_query v.resource_a_r_n)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("ResourceARN", String.to_json v.resource_a_r_n) ])

  let of_json j =
    { resource_a_r_n =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceARN"))
    }
end

module DescribeAlarmsInput = struct
  type t =
    { alarm_names : AlarmNames.t
    ; alarm_name_prefix : String.t option
    ; alarm_types : AlarmTypes.t
    ; children_of_alarm_name : String.t option
    ; parents_of_alarm_name : String.t option
    ; state_value : StateValue.t option
    ; action_prefix : String.t option
    ; max_records : Integer.t option
    ; next_token : String.t option
    }

  let make
      ?(alarm_names = [])
      ?alarm_name_prefix
      ?(alarm_types = [])
      ?children_of_alarm_name
      ?parents_of_alarm_name
      ?state_value
      ?action_prefix
      ?max_records
      ?next_token
      () =
    { alarm_names
    ; alarm_name_prefix
    ; alarm_types
    ; children_of_alarm_name
    ; parents_of_alarm_name
    ; state_value
    ; action_prefix
    ; max_records
    ; next_token
    }

  let parse xml =
    Some
      { alarm_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "AlarmNames" xml) AlarmNames.parse)
      ; alarm_name_prefix =
          Aws.Util.option_bind (Aws.Xml.member "AlarmNamePrefix" xml) String.parse
      ; alarm_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "AlarmTypes" xml) AlarmTypes.parse)
      ; children_of_alarm_name =
          Aws.Util.option_bind (Aws.Xml.member "ChildrenOfAlarmName" xml) String.parse
      ; parents_of_alarm_name =
          Aws.Util.option_bind (Aws.Xml.member "ParentsOfAlarmName" xml) String.parse
      ; state_value =
          Aws.Util.option_bind (Aws.Xml.member "StateValue" xml) StateValue.parse
      ; action_prefix =
          Aws.Util.option_bind (Aws.Xml.member "ActionPrefix" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.action_prefix (fun f ->
               Aws.Query.Pair ("ActionPrefix", String.to_query f))
         ; Aws.Util.option_map v.state_value (fun f ->
               Aws.Query.Pair ("StateValue", StateValue.to_query f))
         ; Aws.Util.option_map v.parents_of_alarm_name (fun f ->
               Aws.Query.Pair ("ParentsOfAlarmName", String.to_query f))
         ; Aws.Util.option_map v.children_of_alarm_name (fun f ->
               Aws.Query.Pair ("ChildrenOfAlarmName", String.to_query f))
         ; Some (Aws.Query.Pair ("AlarmTypes.member", AlarmTypes.to_query v.alarm_types))
         ; Aws.Util.option_map v.alarm_name_prefix (fun f ->
               Aws.Query.Pair ("AlarmNamePrefix", String.to_query f))
         ; Some (Aws.Query.Pair ("AlarmNames.member", AlarmNames.to_query v.alarm_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.action_prefix (fun f -> "ActionPrefix", String.to_json f)
         ; Aws.Util.option_map v.state_value (fun f -> "StateValue", StateValue.to_json f)
         ; Aws.Util.option_map v.parents_of_alarm_name (fun f ->
               "ParentsOfAlarmName", String.to_json f)
         ; Aws.Util.option_map v.children_of_alarm_name (fun f ->
               "ChildrenOfAlarmName", String.to_json f)
         ; Some ("AlarmTypes", AlarmTypes.to_json v.alarm_types)
         ; Aws.Util.option_map v.alarm_name_prefix (fun f ->
               "AlarmNamePrefix", String.to_json f)
         ; Some ("AlarmNames", AlarmNames.to_json v.alarm_names)
         ])

  let of_json j =
    { alarm_names =
        AlarmNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmNames"))
    ; alarm_name_prefix =
        Aws.Util.option_map (Aws.Json.lookup j "AlarmNamePrefix") String.of_json
    ; alarm_types =
        AlarmTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmTypes"))
    ; children_of_alarm_name =
        Aws.Util.option_map (Aws.Json.lookup j "ChildrenOfAlarmName") String.of_json
    ; parents_of_alarm_name =
        Aws.Util.option_map (Aws.Json.lookup j "ParentsOfAlarmName") String.of_json
    ; state_value =
        Aws.Util.option_map (Aws.Json.lookup j "StateValue") StateValue.of_json
    ; action_prefix =
        Aws.Util.option_map (Aws.Json.lookup j "ActionPrefix") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DescribeAlarmHistoryInput = struct
  type t =
    { alarm_name : String.t option
    ; alarm_types : AlarmTypes.t
    ; history_item_type : HistoryItemType.t option
    ; start_date : DateTime.t option
    ; end_date : DateTime.t option
    ; max_records : Integer.t option
    ; next_token : String.t option
    ; scan_by : ScanBy.t option
    }

  let make
      ?alarm_name
      ?(alarm_types = [])
      ?history_item_type
      ?start_date
      ?end_date
      ?max_records
      ?next_token
      ?scan_by
      () =
    { alarm_name
    ; alarm_types
    ; history_item_type
    ; start_date
    ; end_date
    ; max_records
    ; next_token
    ; scan_by
    }

  let parse xml =
    Some
      { alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
      ; alarm_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "AlarmTypes" xml) AlarmTypes.parse)
      ; history_item_type =
          Aws.Util.option_bind
            (Aws.Xml.member "HistoryItemType" xml)
            HistoryItemType.parse
      ; start_date = Aws.Util.option_bind (Aws.Xml.member "StartDate" xml) DateTime.parse
      ; end_date = Aws.Util.option_bind (Aws.Xml.member "EndDate" xml) DateTime.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; scan_by = Aws.Util.option_bind (Aws.Xml.member "ScanBy" xml) ScanBy.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.scan_by (fun f ->
               Aws.Query.Pair ("ScanBy", ScanBy.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.end_date (fun f ->
               Aws.Query.Pair ("EndDate", DateTime.to_query f))
         ; Aws.Util.option_map v.start_date (fun f ->
               Aws.Query.Pair ("StartDate", DateTime.to_query f))
         ; Aws.Util.option_map v.history_item_type (fun f ->
               Aws.Query.Pair ("HistoryItemType", HistoryItemType.to_query f))
         ; Some (Aws.Query.Pair ("AlarmTypes.member", AlarmTypes.to_query v.alarm_types))
         ; Aws.Util.option_map v.alarm_name (fun f ->
               Aws.Query.Pair ("AlarmName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.scan_by (fun f -> "ScanBy", ScanBy.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.end_date (fun f -> "EndDate", DateTime.to_json f)
         ; Aws.Util.option_map v.start_date (fun f -> "StartDate", DateTime.to_json f)
         ; Aws.Util.option_map v.history_item_type (fun f ->
               "HistoryItemType", HistoryItemType.to_json f)
         ; Some ("AlarmTypes", AlarmTypes.to_json v.alarm_types)
         ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
         ])

  let of_json j =
    { alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
    ; alarm_types =
        AlarmTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmTypes"))
    ; history_item_type =
        Aws.Util.option_map (Aws.Json.lookup j "HistoryItemType") HistoryItemType.of_json
    ; start_date = Aws.Util.option_map (Aws.Json.lookup j "StartDate") DateTime.of_json
    ; end_date = Aws.Util.option_map (Aws.Json.lookup j "EndDate") DateTime.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; scan_by = Aws.Util.option_map (Aws.Json.lookup j "ScanBy") ScanBy.of_json
    }
end

module GetInsightRuleReportInput = struct
  type t =
    { rule_name : String.t
    ; start_time : DateTime.t
    ; end_time : DateTime.t
    ; period : Integer.t
    ; max_contributor_count : Integer.t option
    ; metrics : InsightRuleMetricList.t
    ; order_by : String.t option
    }

  let make
      ~rule_name
      ~start_time
      ~end_time
      ~period
      ?max_contributor_count
      ?(metrics = [])
      ?order_by
      () =
    { rule_name; start_time; end_time; period; max_contributor_count; metrics; order_by }

  let parse xml =
    Some
      { rule_name =
          Aws.Xml.required
            "RuleName"
            (Aws.Util.option_bind (Aws.Xml.member "RuleName" xml) String.parse)
      ; start_time =
          Aws.Xml.required
            "StartTime"
            (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
      ; end_time =
          Aws.Xml.required
            "EndTime"
            (Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse)
      ; period =
          Aws.Xml.required
            "Period"
            (Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse)
      ; max_contributor_count =
          Aws.Util.option_bind (Aws.Xml.member "MaxContributorCount" xml) Integer.parse
      ; metrics =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Metrics" xml)
               InsightRuleMetricList.parse)
      ; order_by = Aws.Util.option_bind (Aws.Xml.member "OrderBy" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.order_by (fun f ->
               Aws.Query.Pair ("OrderBy", String.to_query f))
         ; Some
             (Aws.Query.Pair ("Metrics.member", InsightRuleMetricList.to_query v.metrics))
         ; Aws.Util.option_map v.max_contributor_count (fun f ->
               Aws.Query.Pair ("MaxContributorCount", Integer.to_query f))
         ; Some (Aws.Query.Pair ("Period", Integer.to_query v.period))
         ; Some (Aws.Query.Pair ("EndTime", DateTime.to_query v.end_time))
         ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
         ; Some (Aws.Query.Pair ("RuleName", String.to_query v.rule_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.order_by (fun f -> "OrderBy", String.to_json f)
         ; Some ("Metrics", InsightRuleMetricList.to_json v.metrics)
         ; Aws.Util.option_map v.max_contributor_count (fun f ->
               "MaxContributorCount", Integer.to_json f)
         ; Some ("Period", Integer.to_json v.period)
         ; Some ("EndTime", DateTime.to_json v.end_time)
         ; Some ("StartTime", DateTime.to_json v.start_time)
         ; Some ("RuleName", String.to_json v.rule_name)
         ])

  let of_json j =
    { rule_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleName"))
    ; start_time =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
    ; end_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndTime"))
    ; period = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Period"))
    ; max_contributor_count =
        Aws.Util.option_map (Aws.Json.lookup j "MaxContributorCount") Integer.of_json
    ; metrics =
        InsightRuleMetricList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Metrics"))
    ; order_by = Aws.Util.option_map (Aws.Json.lookup j "OrderBy") String.of_json
    }
end

module EnableInsightRulesInput = struct
  type t = { rule_names : InsightRuleNames.t }

  let make ~rule_names () = { rule_names }

  let parse xml =
    Some
      { rule_names =
          Aws.Xml.required
            "RuleNames"
            (Aws.Util.option_bind (Aws.Xml.member "RuleNames" xml) InsightRuleNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("RuleNames.member", InsightRuleNames.to_query v.rule_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("RuleNames", InsightRuleNames.to_json v.rule_names) ])

  let of_json j =
    { rule_names =
        InsightRuleNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleNames"))
    }
end

module ConcurrentModificationException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InternalServiceFault = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("Message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "Message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json }
end

module TagResourceInput = struct
  type t =
    { resource_a_r_n : String.t
    ; tags : TagList.t
    }

  let make ~resource_a_r_n ~tags () = { resource_a_r_n; tags }

  let parse xml =
    Some
      { resource_a_r_n =
          Aws.Xml.required
            "ResourceARN"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceARN" xml) String.parse)
      ; tags =
          Aws.Xml.required
            "Tags"
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Aws.Query.Pair ("ResourceARN", String.to_query v.resource_a_r_n))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Some ("ResourceARN", String.to_json v.resource_a_r_n)
         ])

  let of_json j =
    { resource_a_r_n =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceARN"))
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module RecentlyActive = struct
  type t = PT3H

  let str_to_t = [ "PT3H", PT3H ]

  let t_to_str = [ PT3H, "PT3H" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DimensionFilters = struct
  type t = DimensionFilter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map DimensionFilter.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list DimensionFilter.to_query v

  let to_json v = `List (List.map DimensionFilter.to_json v)

  let of_json j = Aws.Json.to_list DimensionFilter.of_json j
end

module ListMetricsInput = struct
  type t =
    { namespace : String.t option
    ; metric_name : String.t option
    ; dimensions : DimensionFilters.t
    ; next_token : String.t option
    ; recently_active : RecentlyActive.t option
    }

  let make ?namespace ?metric_name ?(dimensions = []) ?next_token ?recently_active () =
    { namespace; metric_name; dimensions; next_token; recently_active }

  let parse xml =
    Some
      { namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
      ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
      ; dimensions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Dimensions" xml)
               DimensionFilters.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; recently_active =
          Aws.Util.option_bind (Aws.Xml.member "RecentlyActive" xml) RecentlyActive.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.recently_active (fun f ->
               Aws.Query.Pair ("RecentlyActive", RecentlyActive.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair ("Dimensions.member", DimensionFilters.to_query v.dimensions))
         ; Aws.Util.option_map v.metric_name (fun f ->
               Aws.Query.Pair ("MetricName", String.to_query f))
         ; Aws.Util.option_map v.namespace (fun f ->
               Aws.Query.Pair ("Namespace", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.recently_active (fun f ->
               "RecentlyActive", RecentlyActive.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Dimensions", DimensionFilters.to_json v.dimensions)
         ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
         ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
         ])

  let of_json j =
    { namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
    ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
    ; dimensions =
        DimensionFilters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; recently_active =
        Aws.Util.option_map (Aws.Json.lookup j "RecentlyActive") RecentlyActive.of_json
    }
end

module GetDashboardInput = struct
  type t = { dashboard_name : String.t }

  let make ~dashboard_name () = { dashboard_name }

  let parse xml =
    Some
      { dashboard_name =
          Aws.Xml.required
            "DashboardName"
            (Aws.Util.option_bind (Aws.Xml.member "DashboardName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("DashboardName", String.to_query v.dashboard_name)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("DashboardName", String.to_json v.dashboard_name) ])

  let of_json j =
    { dashboard_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DashboardName"))
    }
end

module GetDashboardOutput = struct
  type t =
    { dashboard_arn : String.t option
    ; dashboard_body : String.t option
    ; dashboard_name : String.t option
    }

  let make ?dashboard_arn ?dashboard_body ?dashboard_name () =
    { dashboard_arn; dashboard_body; dashboard_name }

  let parse xml =
    Some
      { dashboard_arn =
          Aws.Util.option_bind (Aws.Xml.member "DashboardArn" xml) String.parse
      ; dashboard_body =
          Aws.Util.option_bind (Aws.Xml.member "DashboardBody" xml) String.parse
      ; dashboard_name =
          Aws.Util.option_bind (Aws.Xml.member "DashboardName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.dashboard_name (fun f ->
               Aws.Query.Pair ("DashboardName", String.to_query f))
         ; Aws.Util.option_map v.dashboard_body (fun f ->
               Aws.Query.Pair ("DashboardBody", String.to_query f))
         ; Aws.Util.option_map v.dashboard_arn (fun f ->
               Aws.Query.Pair ("DashboardArn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.dashboard_name (fun f ->
               "DashboardName", String.to_json f)
         ; Aws.Util.option_map v.dashboard_body (fun f ->
               "DashboardBody", String.to_json f)
         ; Aws.Util.option_map v.dashboard_arn (fun f -> "DashboardArn", String.to_json f)
         ])

  let of_json j =
    { dashboard_arn =
        Aws.Util.option_map (Aws.Json.lookup j "DashboardArn") String.of_json
    ; dashboard_body =
        Aws.Util.option_map (Aws.Json.lookup j "DashboardBody") String.of_json
    ; dashboard_name =
        Aws.Util.option_map (Aws.Json.lookup j "DashboardName") String.of_json
    }
end

module DisableInsightRulesOutput = struct
  type t = { failures : BatchFailures.t }

  let make ?(failures = []) () = { failures }

  let parse xml =
    Some
      { failures =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Failures" xml) BatchFailures.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Failures.member", BatchFailures.to_query v.failures)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Failures", BatchFailures.to_json v.failures) ])

  let of_json j =
    { failures =
        BatchFailures.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Failures"))
    }
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
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AlarmHistoryItems" xml)
               AlarmHistoryItems.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "AlarmHistoryItems.member"
                , AlarmHistoryItems.to_query v.alarm_history_items ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("AlarmHistoryItems", AlarmHistoryItems.to_json v.alarm_history_items)
         ])

  let of_json j =
    { alarm_history_items =
        AlarmHistoryItems.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmHistoryItems"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end
