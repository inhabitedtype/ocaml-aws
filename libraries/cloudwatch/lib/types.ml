open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module Dimension =
  struct
    type t = {
      name: String.t ;
      value: String.t }
    let make ~name  ~value  () = { name; value }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (String.to_query v.value)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (String.to_json v.value));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module Dimensions =
  struct
    type t = Dimension.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Dimension.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Dimension.to_query v
    let to_json v = `List (List.map Dimension.to_json v)
    let of_json j = Json.to_list Dimension.of_json j
  end
module Metric =
  struct
    type t =
      {
      namespace: String.t option ;
      metric_name: String.t option ;
      dimensions: Dimensions.t }
    let make ?namespace  ?metric_name  ?(dimensions= [])  () =
      { namespace; metric_name; dimensions }
    let parse xml =
      Some
        {
          namespace =
            (Util.option_bind (Xml.member "Namespace" xml) String.parse);
          metric_name =
            (Util.option_bind (Xml.member "MetricName" xml) String.parse);
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Util.option_map v.metric_name
             (fun f -> Query.Pair ("MetricName", (String.to_query f)));
           Util.option_map v.namespace
             (fun f -> Query.Pair ("Namespace", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("dimensions", (Dimensions.to_json v.dimensions));
           Util.option_map v.metric_name
             (fun f -> ("metric_name", (String.to_json f)));
           Util.option_map v.namespace
             (fun f -> ("namespace", (String.to_json f)))])
    let of_json j =
      {
        namespace =
          (Util.option_map (Json.lookup j "namespace") String.of_json);
        metric_name =
          (Util.option_map (Json.lookup j "metric_name") String.of_json);
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")))
      }
  end
module StandardUnit =
  struct
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
      [("None", None);
      ("Count/Second", Count_Second);
      ("Terabits/Second", Terabits_Second);
      ("Gigabits/Second", Gigabits_Second);
      ("Megabits/Second", Megabits_Second);
      ("Kilobits/Second", Kilobits_Second);
      ("Bits/Second", Bits_Second);
      ("Terabytes/Second", Terabytes_Second);
      ("Gigabytes/Second", Gigabytes_Second);
      ("Megabytes/Second", Megabytes_Second);
      ("Kilobytes/Second", Kilobytes_Second);
      ("Bytes/Second", Bytes_Second);
      ("Count", Count);
      ("Percent", Percent);
      ("Terabits", Terabits);
      ("Gigabits", Gigabits);
      ("Megabits", Megabits);
      ("Kilobits", Kilobits);
      ("Bits", Bits);
      ("Terabytes", Terabytes);
      ("Gigabytes", Gigabytes);
      ("Megabytes", Megabytes);
      ("Kilobytes", Kilobytes);
      ("Bytes", Bytes);
      ("Milliseconds", Milliseconds);
      ("Microseconds", Microseconds);
      ("Seconds", Seconds)]
    let t_to_str =
      [(None, "None");
      (Count_Second, "Count/Second");
      (Terabits_Second, "Terabits/Second");
      (Gigabits_Second, "Gigabits/Second");
      (Megabits_Second, "Megabits/Second");
      (Kilobits_Second, "Kilobits/Second");
      (Bits_Second, "Bits/Second");
      (Terabytes_Second, "Terabytes/Second");
      (Gigabytes_Second, "Gigabytes/Second");
      (Megabytes_Second, "Megabytes/Second");
      (Kilobytes_Second, "Kilobytes/Second");
      (Bytes_Second, "Bytes/Second");
      (Count, "Count");
      (Percent, "Percent");
      (Terabits, "Terabits");
      (Gigabits, "Gigabits");
      (Megabits, "Megabits");
      (Kilobits, "Kilobits");
      (Bits, "Bits");
      (Terabytes, "Terabytes");
      (Gigabytes, "Gigabytes");
      (Megabytes, "Megabytes");
      (Kilobytes, "Kilobytes");
      (Bytes, "Bytes");
      (Milliseconds, "Milliseconds");
      (Microseconds, "Microseconds");
      (Seconds, "Seconds")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Range =
  struct
    type t = {
      start_time: DateTime.t ;
      end_time: DateTime.t }
    let make ~start_time  ~end_time  () = { start_time; end_time }
    let parse xml =
      Some
        {
          start_time =
            (Xml.required "StartTime"
               (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse));
          end_time =
            (Xml.required "EndTime"
               (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("EndTime", (DateTime.to_query v.end_time)));
           Some (Query.Pair ("StartTime", (DateTime.to_query v.start_time)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("end_time", (DateTime.to_json v.end_time));
           Some ("start_time", (DateTime.to_json v.start_time))])
    let of_json j =
      {
        start_time =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "start_time")));
        end_time =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "end_time")))
      }
  end
module MetricStat =
  struct
    type t =
      {
      metric: Metric.t ;
      period: Integer.t ;
      stat: String.t ;
      unit: StandardUnit.t option }
    let make ~metric  ~period  ~stat  ?unit  () =
      { metric; period; stat; unit }
    let parse xml =
      Some
        {
          metric =
            (Xml.required "Metric"
               (Util.option_bind (Xml.member "Metric" xml) Metric.parse));
          period =
            (Xml.required "Period"
               (Util.option_bind (Xml.member "Period" xml) Integer.parse));
          stat =
            (Xml.required "Stat"
               (Util.option_bind (Xml.member "Stat" xml) String.parse));
          unit =
            (Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.unit
              (fun f -> Query.Pair ("Unit", (StandardUnit.to_query f)));
           Some (Query.Pair ("Stat", (String.to_query v.stat)));
           Some (Query.Pair ("Period", (Integer.to_query v.period)));
           Some (Query.Pair ("Metric", (Metric.to_query v.metric)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.unit
              (fun f -> ("unit", (StandardUnit.to_json f)));
           Some ("stat", (String.to_json v.stat));
           Some ("period", (Integer.to_json v.period));
           Some ("metric", (Metric.to_json v.metric))])
    let of_json j =
      {
        metric =
          (Metric.of_json (Util.of_option_exn (Json.lookup j "metric")));
        period =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "period")));
        stat = (String.of_json (Util.of_option_exn (Json.lookup j "stat")));
        unit = (Util.option_map (Json.lookup j "unit") StandardUnit.of_json)
      }
  end
module InsightRuleContributorDatapoint =
  struct
    type t = {
      timestamp: DateTime.t ;
      approximate_value: Double.t }
    let make ~timestamp  ~approximate_value  () =
      { timestamp; approximate_value }
    let parse xml =
      Some
        {
          timestamp =
            (Xml.required "Timestamp"
               (Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse));
          approximate_value =
            (Xml.required "ApproximateValue"
               (Util.option_bind (Xml.member "ApproximateValue" xml)
                  Double.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ApproximateValue", (Double.to_query v.approximate_value)));
           Some (Query.Pair ("Timestamp", (DateTime.to_query v.timestamp)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("approximate_value", (Double.to_json v.approximate_value));
           Some ("timestamp", (DateTime.to_json v.timestamp))])
    let of_json j =
      {
        timestamp =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "timestamp")));
        approximate_value =
          (Double.of_json
             (Util.of_option_exn (Json.lookup j "approximate_value")))
      }
  end
module AnomalyDetectorExcludedTimeRanges =
  struct
    type t = Range.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Range.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Range.to_query v
    let to_json v = `List (List.map Range.to_json v)
    let of_json j = Json.to_list Range.of_json j
  end
module MetricDataQuery =
  struct
    type t =
      {
      id: String.t ;
      metric_stat: MetricStat.t option ;
      expression: String.t option ;
      label: String.t option ;
      return_data: Boolean.t option ;
      period: Integer.t option }
    let make ~id  ?metric_stat  ?expression  ?label  ?return_data  ?period 
      () = { id; metric_stat; expression; label; return_data; period }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          metric_stat =
            (Util.option_bind (Xml.member "MetricStat" xml) MetricStat.parse);
          expression =
            (Util.option_bind (Xml.member "Expression" xml) String.parse);
          label = (Util.option_bind (Xml.member "Label" xml) String.parse);
          return_data =
            (Util.option_bind (Xml.member "ReturnData" xml) Boolean.parse);
          period = (Util.option_bind (Xml.member "Period" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.period
              (fun f -> Query.Pair ("Period", (Integer.to_query f)));
           Util.option_map v.return_data
             (fun f -> Query.Pair ("ReturnData", (Boolean.to_query f)));
           Util.option_map v.label
             (fun f -> Query.Pair ("Label", (String.to_query f)));
           Util.option_map v.expression
             (fun f -> Query.Pair ("Expression", (String.to_query f)));
           Util.option_map v.metric_stat
             (fun f -> Query.Pair ("MetricStat", (MetricStat.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.period
              (fun f -> ("period", (Integer.to_json f)));
           Util.option_map v.return_data
             (fun f -> ("return_data", (Boolean.to_json f)));
           Util.option_map v.label (fun f -> ("label", (String.to_json f)));
           Util.option_map v.expression
             (fun f -> ("expression", (String.to_json f)));
           Util.option_map v.metric_stat
             (fun f -> ("metric_stat", (MetricStat.to_json f)));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        metric_stat =
          (Util.option_map (Json.lookup j "metric_stat") MetricStat.of_json);
        expression =
          (Util.option_map (Json.lookup j "expression") String.of_json);
        label = (Util.option_map (Json.lookup j "label") String.of_json);
        return_data =
          (Util.option_map (Json.lookup j "return_data") Boolean.of_json);
        period = (Util.option_map (Json.lookup j "period") Integer.of_json)
      }
  end
module MessageData =
  struct
    type t = {
      code: String.t option ;
      value: String.t option }
    let make ?code  ?value  () = { code; value }
    let parse xml =
      Some
        {
          code = (Util.option_bind (Xml.member "Code" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.code
             (fun f -> Query.Pair ("Code", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.code (fun f -> ("code", (String.to_json f)))])
    let of_json j =
      {
        code = (Util.option_map (Json.lookup j "code") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module AlarmType =
  struct
    type t =
      | CompositeAlarm 
      | MetricAlarm 
    let str_to_t =
      [("MetricAlarm", MetricAlarm); ("CompositeAlarm", CompositeAlarm)]
    let t_to_str =
      [(MetricAlarm, "MetricAlarm"); (CompositeAlarm, "CompositeAlarm")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module HistoryItemType =
  struct
    type t =
      | ConfigurationUpdate 
      | StateUpdate 
      | Action 
    let str_to_t =
      [("Action", Action);
      ("StateUpdate", StateUpdate);
      ("ConfigurationUpdate", ConfigurationUpdate)]
    let t_to_str =
      [(Action, "Action");
      (StateUpdate, "StateUpdate");
      (ConfigurationUpdate, "ConfigurationUpdate")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module InsightRuleContributorDatapoints =
  struct
    type t = InsightRuleContributorDatapoint.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InsightRuleContributorDatapoint.parse
           (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list InsightRuleContributorDatapoint.to_query v
    let to_json v =
      `List (List.map InsightRuleContributorDatapoint.to_json v)
    let of_json j = Json.to_list InsightRuleContributorDatapoint.of_json j
  end
module InsightRuleContributorKeys =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module AnomalyDetectorConfiguration =
  struct
    type t =
      {
      excluded_time_ranges: AnomalyDetectorExcludedTimeRanges.t ;
      metric_timezone: String.t option }
    let make ?(excluded_time_ranges= [])  ?metric_timezone  () =
      { excluded_time_ranges; metric_timezone }
    let parse xml =
      Some
        {
          excluded_time_ranges =
            (Util.of_option []
               (Util.option_bind (Xml.member "ExcludedTimeRanges" xml)
                  AnomalyDetectorExcludedTimeRanges.parse));
          metric_timezone =
            (Util.option_bind (Xml.member "MetricTimezone" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.metric_timezone
              (fun f -> Query.Pair ("MetricTimezone", (String.to_query f)));
           Some
             (Query.Pair
                ("ExcludedTimeRanges.member",
                  (AnomalyDetectorExcludedTimeRanges.to_query
                     v.excluded_time_ranges)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.metric_timezone
              (fun f -> ("metric_timezone", (String.to_json f)));
           Some
             ("excluded_time_ranges",
               (AnomalyDetectorExcludedTimeRanges.to_json
                  v.excluded_time_ranges))])
    let of_json j =
      {
        excluded_time_ranges =
          (AnomalyDetectorExcludedTimeRanges.of_json
             (Util.of_option_exn (Json.lookup j "excluded_time_ranges")));
        metric_timezone =
          (Util.option_map (Json.lookup j "metric_timezone") String.of_json)
      }
  end
module AnomalyDetectorStateValue =
  struct
    type t =
      | PENDING_TRAINING 
      | TRAINED_INSUFFICIENT_DATA 
      | TRAINED 
    let str_to_t =
      [("TRAINED", TRAINED);
      ("TRAINED_INSUFFICIENT_DATA", TRAINED_INSUFFICIENT_DATA);
      ("PENDING_TRAINING", PENDING_TRAINING)]
    let t_to_str =
      [(TRAINED, "TRAINED");
      (TRAINED_INSUFFICIENT_DATA, "TRAINED_INSUFFICIENT_DATA");
      (PENDING_TRAINING, "PENDING_TRAINING")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ResourceList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module StateValue =
  struct
    type t =
      | OK 
      | ALARM 
      | INSUFFICIENT_DATA 
    let str_to_t =
      [("INSUFFICIENT_DATA", INSUFFICIENT_DATA);
      ("ALARM", ALARM);
      ("OK", OK)]
    let t_to_str =
      [(INSUFFICIENT_DATA, "INSUFFICIENT_DATA");
      (ALARM, "ALARM");
      (OK, "OK")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ComparisonOperator =
  struct
    type t =
      | GreaterThanOrEqualToThreshold 
      | GreaterThanThreshold 
      | LessThanThreshold 
      | LessThanOrEqualToThreshold 
      | LessThanLowerOrGreaterThanUpperThreshold 
      | LessThanLowerThreshold 
      | GreaterThanUpperThreshold 
    let str_to_t =
      [("GreaterThanUpperThreshold", GreaterThanUpperThreshold);
      ("LessThanLowerThreshold", LessThanLowerThreshold);
      ("LessThanLowerOrGreaterThanUpperThreshold",
        LessThanLowerOrGreaterThanUpperThreshold);
      ("LessThanOrEqualToThreshold", LessThanOrEqualToThreshold);
      ("LessThanThreshold", LessThanThreshold);
      ("GreaterThanThreshold", GreaterThanThreshold);
      ("GreaterThanOrEqualToThreshold", GreaterThanOrEqualToThreshold)]
    let t_to_str =
      [(GreaterThanUpperThreshold, "GreaterThanUpperThreshold");
      (LessThanLowerThreshold, "LessThanLowerThreshold");
      (LessThanLowerOrGreaterThanUpperThreshold,
        "LessThanLowerOrGreaterThanUpperThreshold");
      (LessThanOrEqualToThreshold, "LessThanOrEqualToThreshold");
      (LessThanThreshold, "LessThanThreshold");
      (GreaterThanThreshold, "GreaterThanThreshold");
      (GreaterThanOrEqualToThreshold, "GreaterThanOrEqualToThreshold")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module MetricDataQueries =
  struct
    type t = MetricDataQuery.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map MetricDataQuery.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list MetricDataQuery.to_query v
    let to_json v = `List (List.map MetricDataQuery.to_json v)
    let of_json j = Json.to_list MetricDataQuery.of_json j
  end
module Statistic =
  struct
    type t =
      | SampleCount 
      | Average 
      | Sum 
      | Minimum 
      | Maximum 
    let str_to_t =
      [("Maximum", Maximum);
      ("Minimum", Minimum);
      ("Sum", Sum);
      ("Average", Average);
      ("SampleCount", SampleCount)]
    let t_to_str =
      [(Maximum, "Maximum");
      (Minimum, "Minimum");
      (Sum, "Sum");
      (Average, "Average");
      (SampleCount, "SampleCount")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module DatapointValues =
  struct
    type t = Double.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Double.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Double.to_query v
    let to_json v = `List (List.map Double.to_json v)
    let of_json j = Json.to_list Double.of_json j
  end
module MetricDataResultMessages =
  struct
    type t = MessageData.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map MessageData.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list MessageData.to_query v
    let to_json v = `List (List.map MessageData.to_json v)
    let of_json j = Json.to_list MessageData.of_json j
  end
module StatusCode =
  struct
    type t =
      | Complete 
      | InternalError 
      | PartialData 
    let str_to_t =
      [("PartialData", PartialData);
      ("InternalError", InternalError);
      ("Complete", Complete)]
    let t_to_str =
      [(PartialData, "PartialData");
      (InternalError, "InternalError");
      (Complete, "Complete")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Timestamps =
  struct
    type t = DateTime.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map DateTime.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list DateTime.to_query v
    let to_json v = `List (List.map DateTime.to_json v)
    let of_json j = Json.to_list DateTime.of_json j
  end
module Counts =
  struct
    type t = Double.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Double.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Double.to_query v
    let to_json v = `List (List.map Double.to_json v)
    let of_json j = Json.to_list Double.of_json j
  end
module StatisticSet =
  struct
    type t =
      {
      sample_count: Double.t ;
      sum: Double.t ;
      minimum: Double.t ;
      maximum: Double.t }
    let make ~sample_count  ~sum  ~minimum  ~maximum  () =
      { sample_count; sum; minimum; maximum }
    let parse xml =
      Some
        {
          sample_count =
            (Xml.required "SampleCount"
               (Util.option_bind (Xml.member "SampleCount" xml) Double.parse));
          sum =
            (Xml.required "Sum"
               (Util.option_bind (Xml.member "Sum" xml) Double.parse));
          minimum =
            (Xml.required "Minimum"
               (Util.option_bind (Xml.member "Minimum" xml) Double.parse));
          maximum =
            (Xml.required "Maximum"
               (Util.option_bind (Xml.member "Maximum" xml) Double.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Maximum", (Double.to_query v.maximum)));
           Some (Query.Pair ("Minimum", (Double.to_query v.minimum)));
           Some (Query.Pair ("Sum", (Double.to_query v.sum)));
           Some
             (Query.Pair ("SampleCount", (Double.to_query v.sample_count)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("maximum", (Double.to_json v.maximum));
           Some ("minimum", (Double.to_json v.minimum));
           Some ("sum", (Double.to_json v.sum));
           Some ("sample_count", (Double.to_json v.sample_count))])
    let of_json j =
      {
        sample_count =
          (Double.of_json (Util.of_option_exn (Json.lookup j "sample_count")));
        sum = (Double.of_json (Util.of_option_exn (Json.lookup j "sum")));
        minimum =
          (Double.of_json (Util.of_option_exn (Json.lookup j "minimum")));
        maximum =
          (Double.of_json (Util.of_option_exn (Json.lookup j "maximum")))
      }
  end
module Values =
  struct
    type t = Double.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Double.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Double.to_query v
    let to_json v = `List (List.map Double.to_json v)
    let of_json j = Json.to_list Double.of_json j
  end
module DatapointValueMap =
  struct
    type t = (String.t, Double.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string Double.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Double.to_json v)) :: acc)
           v [])
    let of_json j = Json.to_hashtbl String.of_string Double.of_json j
  end
module AlarmHistoryItem =
  struct
    type t =
      {
      alarm_name: String.t option ;
      alarm_type: AlarmType.t option ;
      timestamp: DateTime.t option ;
      history_item_type: HistoryItemType.t option ;
      history_summary: String.t option ;
      history_data: String.t option }
    let make ?alarm_name  ?alarm_type  ?timestamp  ?history_item_type 
      ?history_summary  ?history_data  () =
      {
        alarm_name;
        alarm_type;
        timestamp;
        history_item_type;
        history_summary;
        history_data
      }
    let parse xml =
      Some
        {
          alarm_name =
            (Util.option_bind (Xml.member "AlarmName" xml) String.parse);
          alarm_type =
            (Util.option_bind (Xml.member "AlarmType" xml) AlarmType.parse);
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse);
          history_item_type =
            (Util.option_bind (Xml.member "HistoryItemType" xml)
               HistoryItemType.parse);
          history_summary =
            (Util.option_bind (Xml.member "HistorySummary" xml) String.parse);
          history_data =
            (Util.option_bind (Xml.member "HistoryData" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.history_data
              (fun f -> Query.Pair ("HistoryData", (String.to_query f)));
           Util.option_map v.history_summary
             (fun f -> Query.Pair ("HistorySummary", (String.to_query f)));
           Util.option_map v.history_item_type
             (fun f ->
                Query.Pair ("HistoryItemType", (HistoryItemType.to_query f)));
           Util.option_map v.timestamp
             (fun f -> Query.Pair ("Timestamp", (DateTime.to_query f)));
           Util.option_map v.alarm_type
             (fun f -> Query.Pair ("AlarmType", (AlarmType.to_query f)));
           Util.option_map v.alarm_name
             (fun f -> Query.Pair ("AlarmName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.history_data
              (fun f -> ("history_data", (String.to_json f)));
           Util.option_map v.history_summary
             (fun f -> ("history_summary", (String.to_json f)));
           Util.option_map v.history_item_type
             (fun f -> ("history_item_type", (HistoryItemType.to_json f)));
           Util.option_map v.timestamp
             (fun f -> ("timestamp", (DateTime.to_json f)));
           Util.option_map v.alarm_type
             (fun f -> ("alarm_type", (AlarmType.to_json f)));
           Util.option_map v.alarm_name
             (fun f -> ("alarm_name", (String.to_json f)))])
    let of_json j =
      {
        alarm_name =
          (Util.option_map (Json.lookup j "alarm_name") String.of_json);
        alarm_type =
          (Util.option_map (Json.lookup j "alarm_type") AlarmType.of_json);
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") DateTime.of_json);
        history_item_type =
          (Util.option_map (Json.lookup j "history_item_type")
             HistoryItemType.of_json);
        history_summary =
          (Util.option_map (Json.lookup j "history_summary") String.of_json);
        history_data =
          (Util.option_map (Json.lookup j "history_data") String.of_json)
      }
  end
module PartialFailure =
  struct
    type t =
      {
      failure_resource: String.t option ;
      exception_type: String.t option ;
      failure_code: String.t option ;
      failure_description: String.t option }
    let make ?failure_resource  ?exception_type  ?failure_code 
      ?failure_description  () =
      { failure_resource; exception_type; failure_code; failure_description }
    let parse xml =
      Some
        {
          failure_resource =
            (Util.option_bind (Xml.member "FailureResource" xml) String.parse);
          exception_type =
            (Util.option_bind (Xml.member "ExceptionType" xml) String.parse);
          failure_code =
            (Util.option_bind (Xml.member "FailureCode" xml) String.parse);
          failure_description =
            (Util.option_bind (Xml.member "FailureDescription" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.failure_description
              (fun f ->
                 Query.Pair ("FailureDescription", (String.to_query f)));
           Util.option_map v.failure_code
             (fun f -> Query.Pair ("FailureCode", (String.to_query f)));
           Util.option_map v.exception_type
             (fun f -> Query.Pair ("ExceptionType", (String.to_query f)));
           Util.option_map v.failure_resource
             (fun f -> Query.Pair ("FailureResource", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.failure_description
              (fun f -> ("failure_description", (String.to_json f)));
           Util.option_map v.failure_code
             (fun f -> ("failure_code", (String.to_json f)));
           Util.option_map v.exception_type
             (fun f -> ("exception_type", (String.to_json f)));
           Util.option_map v.failure_resource
             (fun f -> ("failure_resource", (String.to_json f)))])
    let of_json j =
      {
        failure_resource =
          (Util.option_map (Json.lookup j "failure_resource") String.of_json);
        exception_type =
          (Util.option_map (Json.lookup j "exception_type") String.of_json);
        failure_code =
          (Util.option_map (Json.lookup j "failure_code") String.of_json);
        failure_description =
          (Util.option_map (Json.lookup j "failure_description")
             String.of_json)
      }
  end
module DimensionFilter =
  struct
    type t = {
      name: String.t ;
      value: String.t option }
    let make ~name  ?value  () = { name; value }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module Tag =
  struct
    type t = {
      key: String.t ;
      value: String.t }
    let make ~key  ~value  () = { key; value }
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (String.to_query v.value)));
           Some (Query.Pair ("Key", (String.to_query v.key)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (String.to_json v.value));
           Some ("key", (String.to_json v.key))])
    let of_json j =
      {
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module InsightRuleContributor =
  struct
    type t =
      {
      keys: InsightRuleContributorKeys.t ;
      approximate_aggregate_value: Double.t ;
      datapoints: InsightRuleContributorDatapoints.t }
    let make ~keys  ~approximate_aggregate_value  ~datapoints  () =
      { keys; approximate_aggregate_value; datapoints }
    let parse xml =
      Some
        {
          keys =
            (Xml.required "Keys"
               (Util.option_bind (Xml.member "Keys" xml)
                  InsightRuleContributorKeys.parse));
          approximate_aggregate_value =
            (Xml.required "ApproximateAggregateValue"
               (Util.option_bind (Xml.member "ApproximateAggregateValue" xml)
                  Double.parse));
          datapoints =
            (Xml.required "Datapoints"
               (Util.option_bind (Xml.member "Datapoints" xml)
                  InsightRuleContributorDatapoints.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Datapoints.member",
                   (InsightRuleContributorDatapoints.to_query v.datapoints)));
           Some
             (Query.Pair
                ("ApproximateAggregateValue",
                  (Double.to_query v.approximate_aggregate_value)));
           Some
             (Query.Pair
                ("Keys.member", (InsightRuleContributorKeys.to_query v.keys)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("datapoints",
                (InsightRuleContributorDatapoints.to_json v.datapoints));
           Some
             ("approximate_aggregate_value",
               (Double.to_json v.approximate_aggregate_value));
           Some ("keys", (InsightRuleContributorKeys.to_json v.keys))])
    let of_json j =
      {
        keys =
          (InsightRuleContributorKeys.of_json
             (Util.of_option_exn (Json.lookup j "keys")));
        approximate_aggregate_value =
          (Double.of_json
             (Util.of_option_exn
                (Json.lookup j "approximate_aggregate_value")));
        datapoints =
          (InsightRuleContributorDatapoints.of_json
             (Util.of_option_exn (Json.lookup j "datapoints")))
      }
  end
module InsightRuleMetricDatapoint =
  struct
    type t =
      {
      timestamp: DateTime.t ;
      unique_contributors: Double.t option ;
      max_contributor_value: Double.t option ;
      sample_count: Double.t option ;
      average: Double.t option ;
      sum: Double.t option ;
      minimum: Double.t option ;
      maximum: Double.t option }
    let make ~timestamp  ?unique_contributors  ?max_contributor_value 
      ?sample_count  ?average  ?sum  ?minimum  ?maximum  () =
      {
        timestamp;
        unique_contributors;
        max_contributor_value;
        sample_count;
        average;
        sum;
        minimum;
        maximum
      }
    let parse xml =
      Some
        {
          timestamp =
            (Xml.required "Timestamp"
               (Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse));
          unique_contributors =
            (Util.option_bind (Xml.member "UniqueContributors" xml)
               Double.parse);
          max_contributor_value =
            (Util.option_bind (Xml.member "MaxContributorValue" xml)
               Double.parse);
          sample_count =
            (Util.option_bind (Xml.member "SampleCount" xml) Double.parse);
          average =
            (Util.option_bind (Xml.member "Average" xml) Double.parse);
          sum = (Util.option_bind (Xml.member "Sum" xml) Double.parse);
          minimum =
            (Util.option_bind (Xml.member "Minimum" xml) Double.parse);
          maximum =
            (Util.option_bind (Xml.member "Maximum" xml) Double.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.maximum
              (fun f -> Query.Pair ("Maximum", (Double.to_query f)));
           Util.option_map v.minimum
             (fun f -> Query.Pair ("Minimum", (Double.to_query f)));
           Util.option_map v.sum
             (fun f -> Query.Pair ("Sum", (Double.to_query f)));
           Util.option_map v.average
             (fun f -> Query.Pair ("Average", (Double.to_query f)));
           Util.option_map v.sample_count
             (fun f -> Query.Pair ("SampleCount", (Double.to_query f)));
           Util.option_map v.max_contributor_value
             (fun f ->
                Query.Pair ("MaxContributorValue", (Double.to_query f)));
           Util.option_map v.unique_contributors
             (fun f -> Query.Pair ("UniqueContributors", (Double.to_query f)));
           Some (Query.Pair ("Timestamp", (DateTime.to_query v.timestamp)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.maximum
              (fun f -> ("maximum", (Double.to_json f)));
           Util.option_map v.minimum
             (fun f -> ("minimum", (Double.to_json f)));
           Util.option_map v.sum (fun f -> ("sum", (Double.to_json f)));
           Util.option_map v.average
             (fun f -> ("average", (Double.to_json f)));
           Util.option_map v.sample_count
             (fun f -> ("sample_count", (Double.to_json f)));
           Util.option_map v.max_contributor_value
             (fun f -> ("max_contributor_value", (Double.to_json f)));
           Util.option_map v.unique_contributors
             (fun f -> ("unique_contributors", (Double.to_json f)));
           Some ("timestamp", (DateTime.to_json v.timestamp))])
    let of_json j =
      {
        timestamp =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "timestamp")));
        unique_contributors =
          (Util.option_map (Json.lookup j "unique_contributors")
             Double.of_json);
        max_contributor_value =
          (Util.option_map (Json.lookup j "max_contributor_value")
             Double.of_json);
        sample_count =
          (Util.option_map (Json.lookup j "sample_count") Double.of_json);
        average = (Util.option_map (Json.lookup j "average") Double.of_json);
        sum = (Util.option_map (Json.lookup j "sum") Double.of_json);
        minimum = (Util.option_map (Json.lookup j "minimum") Double.of_json);
        maximum = (Util.option_map (Json.lookup j "maximum") Double.of_json)
      }
  end
module DashboardValidationMessage =
  struct
    type t = {
      data_path: String.t option ;
      message: String.t option }
    let make ?data_path  ?message  () = { data_path; message }
    let parse xml =
      Some
        {
          data_path =
            (Util.option_bind (Xml.member "DataPath" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.data_path
             (fun f -> Query.Pair ("DataPath", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.data_path
             (fun f -> ("data_path", (String.to_json f)))])
    let of_json j =
      {
        data_path =
          (Util.option_map (Json.lookup j "data_path") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module AnomalyDetector =
  struct
    type t =
      {
      namespace: String.t option ;
      metric_name: String.t option ;
      dimensions: Dimensions.t ;
      stat: String.t option ;
      configuration: AnomalyDetectorConfiguration.t option ;
      state_value: AnomalyDetectorStateValue.t option }
    let make ?namespace  ?metric_name  ?(dimensions= [])  ?stat 
      ?configuration  ?state_value  () =
      { namespace; metric_name; dimensions; stat; configuration; state_value
      }
    let parse xml =
      Some
        {
          namespace =
            (Util.option_bind (Xml.member "Namespace" xml) String.parse);
          metric_name =
            (Util.option_bind (Xml.member "MetricName" xml) String.parse);
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse));
          stat = (Util.option_bind (Xml.member "Stat" xml) String.parse);
          configuration =
            (Util.option_bind (Xml.member "Configuration" xml)
               AnomalyDetectorConfiguration.parse);
          state_value =
            (Util.option_bind (Xml.member "StateValue" xml)
               AnomalyDetectorStateValue.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.state_value
              (fun f ->
                 Query.Pair
                   ("StateValue", (AnomalyDetectorStateValue.to_query f)));
           Util.option_map v.configuration
             (fun f ->
                Query.Pair
                  ("Configuration",
                    (AnomalyDetectorConfiguration.to_query f)));
           Util.option_map v.stat
             (fun f -> Query.Pair ("Stat", (String.to_query f)));
           Some
             (Query.Pair
                ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Util.option_map v.metric_name
             (fun f -> Query.Pair ("MetricName", (String.to_query f)));
           Util.option_map v.namespace
             (fun f -> Query.Pair ("Namespace", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.state_value
              (fun f ->
                 ("state_value", (AnomalyDetectorStateValue.to_json f)));
           Util.option_map v.configuration
             (fun f ->
                ("configuration", (AnomalyDetectorConfiguration.to_json f)));
           Util.option_map v.stat (fun f -> ("stat", (String.to_json f)));
           Some ("dimensions", (Dimensions.to_json v.dimensions));
           Util.option_map v.metric_name
             (fun f -> ("metric_name", (String.to_json f)));
           Util.option_map v.namespace
             (fun f -> ("namespace", (String.to_json f)))])
    let of_json j =
      {
        namespace =
          (Util.option_map (Json.lookup j "namespace") String.of_json);
        metric_name =
          (Util.option_map (Json.lookup j "metric_name") String.of_json);
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")));
        stat = (Util.option_map (Json.lookup j "stat") String.of_json);
        configuration =
          (Util.option_map (Json.lookup j "configuration")
             AnomalyDetectorConfiguration.of_json);
        state_value =
          (Util.option_map (Json.lookup j "state_value")
             AnomalyDetectorStateValue.of_json)
      }
  end
module CompositeAlarm =
  struct
    type t =
      {
      actions_enabled: Boolean.t option ;
      alarm_actions: ResourceList.t ;
      alarm_arn: String.t option ;
      alarm_configuration_updated_timestamp: DateTime.t option ;
      alarm_description: String.t option ;
      alarm_name: String.t option ;
      alarm_rule: String.t option ;
      insufficient_data_actions: ResourceList.t ;
      o_k_actions: ResourceList.t ;
      state_reason: String.t option ;
      state_reason_data: String.t option ;
      state_updated_timestamp: DateTime.t option ;
      state_value: StateValue.t option }
    let make ?actions_enabled  ?(alarm_actions= [])  ?alarm_arn 
      ?alarm_configuration_updated_timestamp  ?alarm_description  ?alarm_name
       ?alarm_rule  ?(insufficient_data_actions= [])  ?(o_k_actions= []) 
      ?state_reason  ?state_reason_data  ?state_updated_timestamp 
      ?state_value  () =
      {
        actions_enabled;
        alarm_actions;
        alarm_arn;
        alarm_configuration_updated_timestamp;
        alarm_description;
        alarm_name;
        alarm_rule;
        insufficient_data_actions;
        o_k_actions;
        state_reason;
        state_reason_data;
        state_updated_timestamp;
        state_value
      }
    let parse xml =
      Some
        {
          actions_enabled =
            (Util.option_bind (Xml.member "ActionsEnabled" xml) Boolean.parse);
          alarm_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "AlarmActions" xml)
                  ResourceList.parse));
          alarm_arn =
            (Util.option_bind (Xml.member "AlarmArn" xml) String.parse);
          alarm_configuration_updated_timestamp =
            (Util.option_bind
               (Xml.member "AlarmConfigurationUpdatedTimestamp" xml)
               DateTime.parse);
          alarm_description =
            (Util.option_bind (Xml.member "AlarmDescription" xml)
               String.parse);
          alarm_name =
            (Util.option_bind (Xml.member "AlarmName" xml) String.parse);
          alarm_rule =
            (Util.option_bind (Xml.member "AlarmRule" xml) String.parse);
          insufficient_data_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "InsufficientDataActions" xml)
                  ResourceList.parse));
          o_k_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "OKActions" xml)
                  ResourceList.parse));
          state_reason =
            (Util.option_bind (Xml.member "StateReason" xml) String.parse);
          state_reason_data =
            (Util.option_bind (Xml.member "StateReasonData" xml) String.parse);
          state_updated_timestamp =
            (Util.option_bind (Xml.member "StateUpdatedTimestamp" xml)
               DateTime.parse);
          state_value =
            (Util.option_bind (Xml.member "StateValue" xml) StateValue.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.state_value
              (fun f -> Query.Pair ("StateValue", (StateValue.to_query f)));
           Util.option_map v.state_updated_timestamp
             (fun f ->
                Query.Pair ("StateUpdatedTimestamp", (DateTime.to_query f)));
           Util.option_map v.state_reason_data
             (fun f -> Query.Pair ("StateReasonData", (String.to_query f)));
           Util.option_map v.state_reason
             (fun f -> Query.Pair ("StateReason", (String.to_query f)));
           Some
             (Query.Pair
                ("OKActions.member", (ResourceList.to_query v.o_k_actions)));
           Some
             (Query.Pair
                ("InsufficientDataActions.member",
                  (ResourceList.to_query v.insufficient_data_actions)));
           Util.option_map v.alarm_rule
             (fun f -> Query.Pair ("AlarmRule", (String.to_query f)));
           Util.option_map v.alarm_name
             (fun f -> Query.Pair ("AlarmName", (String.to_query f)));
           Util.option_map v.alarm_description
             (fun f -> Query.Pair ("AlarmDescription", (String.to_query f)));
           Util.option_map v.alarm_configuration_updated_timestamp
             (fun f ->
                Query.Pair
                  ("AlarmConfigurationUpdatedTimestamp",
                    (DateTime.to_query f)));
           Util.option_map v.alarm_arn
             (fun f -> Query.Pair ("AlarmArn", (String.to_query f)));
           Some
             (Query.Pair
                ("AlarmActions.member",
                  (ResourceList.to_query v.alarm_actions)));
           Util.option_map v.actions_enabled
             (fun f -> Query.Pair ("ActionsEnabled", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.state_value
              (fun f -> ("state_value", (StateValue.to_json f)));
           Util.option_map v.state_updated_timestamp
             (fun f -> ("state_updated_timestamp", (DateTime.to_json f)));
           Util.option_map v.state_reason_data
             (fun f -> ("state_reason_data", (String.to_json f)));
           Util.option_map v.state_reason
             (fun f -> ("state_reason", (String.to_json f)));
           Some ("o_k_actions", (ResourceList.to_json v.o_k_actions));
           Some
             ("insufficient_data_actions",
               (ResourceList.to_json v.insufficient_data_actions));
           Util.option_map v.alarm_rule
             (fun f -> ("alarm_rule", (String.to_json f)));
           Util.option_map v.alarm_name
             (fun f -> ("alarm_name", (String.to_json f)));
           Util.option_map v.alarm_description
             (fun f -> ("alarm_description", (String.to_json f)));
           Util.option_map v.alarm_configuration_updated_timestamp
             (fun f ->
                ("alarm_configuration_updated_timestamp",
                  (DateTime.to_json f)));
           Util.option_map v.alarm_arn
             (fun f -> ("alarm_arn", (String.to_json f)));
           Some ("alarm_actions", (ResourceList.to_json v.alarm_actions));
           Util.option_map v.actions_enabled
             (fun f -> ("actions_enabled", (Boolean.to_json f)))])
    let of_json j =
      {
        actions_enabled =
          (Util.option_map (Json.lookup j "actions_enabled") Boolean.of_json);
        alarm_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "alarm_actions")));
        alarm_arn =
          (Util.option_map (Json.lookup j "alarm_arn") String.of_json);
        alarm_configuration_updated_timestamp =
          (Util.option_map
             (Json.lookup j "alarm_configuration_updated_timestamp")
             DateTime.of_json);
        alarm_description =
          (Util.option_map (Json.lookup j "alarm_description") String.of_json);
        alarm_name =
          (Util.option_map (Json.lookup j "alarm_name") String.of_json);
        alarm_rule =
          (Util.option_map (Json.lookup j "alarm_rule") String.of_json);
        insufficient_data_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "insufficient_data_actions")));
        o_k_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "o_k_actions")));
        state_reason =
          (Util.option_map (Json.lookup j "state_reason") String.of_json);
        state_reason_data =
          (Util.option_map (Json.lookup j "state_reason_data") String.of_json);
        state_updated_timestamp =
          (Util.option_map (Json.lookup j "state_updated_timestamp")
             DateTime.of_json);
        state_value =
          (Util.option_map (Json.lookup j "state_value") StateValue.of_json)
      }
  end
module MetricAlarm =
  struct
    type t =
      {
      alarm_name: String.t option ;
      alarm_arn: String.t option ;
      alarm_description: String.t option ;
      alarm_configuration_updated_timestamp: DateTime.t option ;
      actions_enabled: Boolean.t option ;
      o_k_actions: ResourceList.t ;
      alarm_actions: ResourceList.t ;
      insufficient_data_actions: ResourceList.t ;
      state_value: StateValue.t option ;
      state_reason: String.t option ;
      state_reason_data: String.t option ;
      state_updated_timestamp: DateTime.t option ;
      metric_name: String.t option ;
      namespace: String.t option ;
      statistic: Statistic.t option ;
      extended_statistic: String.t option ;
      dimensions: Dimensions.t ;
      period: Integer.t option ;
      unit: StandardUnit.t option ;
      evaluation_periods: Integer.t option ;
      datapoints_to_alarm: Integer.t option ;
      threshold: Double.t option ;
      comparison_operator: ComparisonOperator.t option ;
      treat_missing_data: String.t option ;
      evaluate_low_sample_count_percentile: String.t option ;
      metrics: MetricDataQueries.t ;
      threshold_metric_id: String.t option }
    let make ?alarm_name  ?alarm_arn  ?alarm_description 
      ?alarm_configuration_updated_timestamp  ?actions_enabled 
      ?(o_k_actions= [])  ?(alarm_actions= [])  ?(insufficient_data_actions=
      [])  ?state_value  ?state_reason  ?state_reason_data 
      ?state_updated_timestamp  ?metric_name  ?namespace  ?statistic 
      ?extended_statistic  ?(dimensions= [])  ?period  ?unit 
      ?evaluation_periods  ?datapoints_to_alarm  ?threshold 
      ?comparison_operator  ?treat_missing_data 
      ?evaluate_low_sample_count_percentile  ?(metrics= []) 
      ?threshold_metric_id  () =
      {
        alarm_name;
        alarm_arn;
        alarm_description;
        alarm_configuration_updated_timestamp;
        actions_enabled;
        o_k_actions;
        alarm_actions;
        insufficient_data_actions;
        state_value;
        state_reason;
        state_reason_data;
        state_updated_timestamp;
        metric_name;
        namespace;
        statistic;
        extended_statistic;
        dimensions;
        period;
        unit;
        evaluation_periods;
        datapoints_to_alarm;
        threshold;
        comparison_operator;
        treat_missing_data;
        evaluate_low_sample_count_percentile;
        metrics;
        threshold_metric_id
      }
    let parse xml =
      Some
        {
          alarm_name =
            (Util.option_bind (Xml.member "AlarmName" xml) String.parse);
          alarm_arn =
            (Util.option_bind (Xml.member "AlarmArn" xml) String.parse);
          alarm_description =
            (Util.option_bind (Xml.member "AlarmDescription" xml)
               String.parse);
          alarm_configuration_updated_timestamp =
            (Util.option_bind
               (Xml.member "AlarmConfigurationUpdatedTimestamp" xml)
               DateTime.parse);
          actions_enabled =
            (Util.option_bind (Xml.member "ActionsEnabled" xml) Boolean.parse);
          o_k_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "OKActions" xml)
                  ResourceList.parse));
          alarm_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "AlarmActions" xml)
                  ResourceList.parse));
          insufficient_data_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "InsufficientDataActions" xml)
                  ResourceList.parse));
          state_value =
            (Util.option_bind (Xml.member "StateValue" xml) StateValue.parse);
          state_reason =
            (Util.option_bind (Xml.member "StateReason" xml) String.parse);
          state_reason_data =
            (Util.option_bind (Xml.member "StateReasonData" xml) String.parse);
          state_updated_timestamp =
            (Util.option_bind (Xml.member "StateUpdatedTimestamp" xml)
               DateTime.parse);
          metric_name =
            (Util.option_bind (Xml.member "MetricName" xml) String.parse);
          namespace =
            (Util.option_bind (Xml.member "Namespace" xml) String.parse);
          statistic =
            (Util.option_bind (Xml.member "Statistic" xml) Statistic.parse);
          extended_statistic =
            (Util.option_bind (Xml.member "ExtendedStatistic" xml)
               String.parse);
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse));
          period = (Util.option_bind (Xml.member "Period" xml) Integer.parse);
          unit =
            (Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse);
          evaluation_periods =
            (Util.option_bind (Xml.member "EvaluationPeriods" xml)
               Integer.parse);
          datapoints_to_alarm =
            (Util.option_bind (Xml.member "DatapointsToAlarm" xml)
               Integer.parse);
          threshold =
            (Util.option_bind (Xml.member "Threshold" xml) Double.parse);
          comparison_operator =
            (Util.option_bind (Xml.member "ComparisonOperator" xml)
               ComparisonOperator.parse);
          treat_missing_data =
            (Util.option_bind (Xml.member "TreatMissingData" xml)
               String.parse);
          evaluate_low_sample_count_percentile =
            (Util.option_bind
               (Xml.member "EvaluateLowSampleCountPercentile" xml)
               String.parse);
          metrics =
            (Util.of_option []
               (Util.option_bind (Xml.member "Metrics" xml)
                  MetricDataQueries.parse));
          threshold_metric_id =
            (Util.option_bind (Xml.member "ThresholdMetricId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.threshold_metric_id
              (fun f -> Query.Pair ("ThresholdMetricId", (String.to_query f)));
           Some
             (Query.Pair
                ("Metrics.member", (MetricDataQueries.to_query v.metrics)));
           Util.option_map v.evaluate_low_sample_count_percentile
             (fun f ->
                Query.Pair
                  ("EvaluateLowSampleCountPercentile", (String.to_query f)));
           Util.option_map v.treat_missing_data
             (fun f -> Query.Pair ("TreatMissingData", (String.to_query f)));
           Util.option_map v.comparison_operator
             (fun f ->
                Query.Pair
                  ("ComparisonOperator", (ComparisonOperator.to_query f)));
           Util.option_map v.threshold
             (fun f -> Query.Pair ("Threshold", (Double.to_query f)));
           Util.option_map v.datapoints_to_alarm
             (fun f -> Query.Pair ("DatapointsToAlarm", (Integer.to_query f)));
           Util.option_map v.evaluation_periods
             (fun f -> Query.Pair ("EvaluationPeriods", (Integer.to_query f)));
           Util.option_map v.unit
             (fun f -> Query.Pair ("Unit", (StandardUnit.to_query f)));
           Util.option_map v.period
             (fun f -> Query.Pair ("Period", (Integer.to_query f)));
           Some
             (Query.Pair
                ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Util.option_map v.extended_statistic
             (fun f -> Query.Pair ("ExtendedStatistic", (String.to_query f)));
           Util.option_map v.statistic
             (fun f -> Query.Pair ("Statistic", (Statistic.to_query f)));
           Util.option_map v.namespace
             (fun f -> Query.Pair ("Namespace", (String.to_query f)));
           Util.option_map v.metric_name
             (fun f -> Query.Pair ("MetricName", (String.to_query f)));
           Util.option_map v.state_updated_timestamp
             (fun f ->
                Query.Pair ("StateUpdatedTimestamp", (DateTime.to_query f)));
           Util.option_map v.state_reason_data
             (fun f -> Query.Pair ("StateReasonData", (String.to_query f)));
           Util.option_map v.state_reason
             (fun f -> Query.Pair ("StateReason", (String.to_query f)));
           Util.option_map v.state_value
             (fun f -> Query.Pair ("StateValue", (StateValue.to_query f)));
           Some
             (Query.Pair
                ("InsufficientDataActions.member",
                  (ResourceList.to_query v.insufficient_data_actions)));
           Some
             (Query.Pair
                ("AlarmActions.member",
                  (ResourceList.to_query v.alarm_actions)));
           Some
             (Query.Pair
                ("OKActions.member", (ResourceList.to_query v.o_k_actions)));
           Util.option_map v.actions_enabled
             (fun f -> Query.Pair ("ActionsEnabled", (Boolean.to_query f)));
           Util.option_map v.alarm_configuration_updated_timestamp
             (fun f ->
                Query.Pair
                  ("AlarmConfigurationUpdatedTimestamp",
                    (DateTime.to_query f)));
           Util.option_map v.alarm_description
             (fun f -> Query.Pair ("AlarmDescription", (String.to_query f)));
           Util.option_map v.alarm_arn
             (fun f -> Query.Pair ("AlarmArn", (String.to_query f)));
           Util.option_map v.alarm_name
             (fun f -> Query.Pair ("AlarmName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.threshold_metric_id
              (fun f -> ("threshold_metric_id", (String.to_json f)));
           Some ("metrics", (MetricDataQueries.to_json v.metrics));
           Util.option_map v.evaluate_low_sample_count_percentile
             (fun f ->
                ("evaluate_low_sample_count_percentile", (String.to_json f)));
           Util.option_map v.treat_missing_data
             (fun f -> ("treat_missing_data", (String.to_json f)));
           Util.option_map v.comparison_operator
             (fun f ->
                ("comparison_operator", (ComparisonOperator.to_json f)));
           Util.option_map v.threshold
             (fun f -> ("threshold", (Double.to_json f)));
           Util.option_map v.datapoints_to_alarm
             (fun f -> ("datapoints_to_alarm", (Integer.to_json f)));
           Util.option_map v.evaluation_periods
             (fun f -> ("evaluation_periods", (Integer.to_json f)));
           Util.option_map v.unit
             (fun f -> ("unit", (StandardUnit.to_json f)));
           Util.option_map v.period
             (fun f -> ("period", (Integer.to_json f)));
           Some ("dimensions", (Dimensions.to_json v.dimensions));
           Util.option_map v.extended_statistic
             (fun f -> ("extended_statistic", (String.to_json f)));
           Util.option_map v.statistic
             (fun f -> ("statistic", (Statistic.to_json f)));
           Util.option_map v.namespace
             (fun f -> ("namespace", (String.to_json f)));
           Util.option_map v.metric_name
             (fun f -> ("metric_name", (String.to_json f)));
           Util.option_map v.state_updated_timestamp
             (fun f -> ("state_updated_timestamp", (DateTime.to_json f)));
           Util.option_map v.state_reason_data
             (fun f -> ("state_reason_data", (String.to_json f)));
           Util.option_map v.state_reason
             (fun f -> ("state_reason", (String.to_json f)));
           Util.option_map v.state_value
             (fun f -> ("state_value", (StateValue.to_json f)));
           Some
             ("insufficient_data_actions",
               (ResourceList.to_json v.insufficient_data_actions));
           Some ("alarm_actions", (ResourceList.to_json v.alarm_actions));
           Some ("o_k_actions", (ResourceList.to_json v.o_k_actions));
           Util.option_map v.actions_enabled
             (fun f -> ("actions_enabled", (Boolean.to_json f)));
           Util.option_map v.alarm_configuration_updated_timestamp
             (fun f ->
                ("alarm_configuration_updated_timestamp",
                  (DateTime.to_json f)));
           Util.option_map v.alarm_description
             (fun f -> ("alarm_description", (String.to_json f)));
           Util.option_map v.alarm_arn
             (fun f -> ("alarm_arn", (String.to_json f)));
           Util.option_map v.alarm_name
             (fun f -> ("alarm_name", (String.to_json f)))])
    let of_json j =
      {
        alarm_name =
          (Util.option_map (Json.lookup j "alarm_name") String.of_json);
        alarm_arn =
          (Util.option_map (Json.lookup j "alarm_arn") String.of_json);
        alarm_description =
          (Util.option_map (Json.lookup j "alarm_description") String.of_json);
        alarm_configuration_updated_timestamp =
          (Util.option_map
             (Json.lookup j "alarm_configuration_updated_timestamp")
             DateTime.of_json);
        actions_enabled =
          (Util.option_map (Json.lookup j "actions_enabled") Boolean.of_json);
        o_k_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "o_k_actions")));
        alarm_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "alarm_actions")));
        insufficient_data_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "insufficient_data_actions")));
        state_value =
          (Util.option_map (Json.lookup j "state_value") StateValue.of_json);
        state_reason =
          (Util.option_map (Json.lookup j "state_reason") String.of_json);
        state_reason_data =
          (Util.option_map (Json.lookup j "state_reason_data") String.of_json);
        state_updated_timestamp =
          (Util.option_map (Json.lookup j "state_updated_timestamp")
             DateTime.of_json);
        metric_name =
          (Util.option_map (Json.lookup j "metric_name") String.of_json);
        namespace =
          (Util.option_map (Json.lookup j "namespace") String.of_json);
        statistic =
          (Util.option_map (Json.lookup j "statistic") Statistic.of_json);
        extended_statistic =
          (Util.option_map (Json.lookup j "extended_statistic")
             String.of_json);
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")));
        period = (Util.option_map (Json.lookup j "period") Integer.of_json);
        unit = (Util.option_map (Json.lookup j "unit") StandardUnit.of_json);
        evaluation_periods =
          (Util.option_map (Json.lookup j "evaluation_periods")
             Integer.of_json);
        datapoints_to_alarm =
          (Util.option_map (Json.lookup j "datapoints_to_alarm")
             Integer.of_json);
        threshold =
          (Util.option_map (Json.lookup j "threshold") Double.of_json);
        comparison_operator =
          (Util.option_map (Json.lookup j "comparison_operator")
             ComparisonOperator.of_json);
        treat_missing_data =
          (Util.option_map (Json.lookup j "treat_missing_data")
             String.of_json);
        evaluate_low_sample_count_percentile =
          (Util.option_map
             (Json.lookup j "evaluate_low_sample_count_percentile")
             String.of_json);
        metrics =
          (MetricDataQueries.of_json
             (Util.of_option_exn (Json.lookup j "metrics")));
        threshold_metric_id =
          (Util.option_map (Json.lookup j "threshold_metric_id")
             String.of_json)
      }
  end
module MetricDataResult =
  struct
    type t =
      {
      id: String.t option ;
      label: String.t option ;
      timestamps: Timestamps.t ;
      values: DatapointValues.t ;
      status_code: StatusCode.t option ;
      messages: MetricDataResultMessages.t }
    let make ?id  ?label  ?(timestamps= [])  ?(values= [])  ?status_code 
      ?(messages= [])  () =
      { id; label; timestamps; values; status_code; messages }
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          label = (Util.option_bind (Xml.member "Label" xml) String.parse);
          timestamps =
            (Util.of_option []
               (Util.option_bind (Xml.member "Timestamps" xml)
                  Timestamps.parse));
          values =
            (Util.of_option []
               (Util.option_bind (Xml.member "Values" xml)
                  DatapointValues.parse));
          status_code =
            (Util.option_bind (Xml.member "StatusCode" xml) StatusCode.parse);
          messages =
            (Util.of_option []
               (Util.option_bind (Xml.member "Messages" xml)
                  MetricDataResultMessages.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Messages.member",
                   (MetricDataResultMessages.to_query v.messages)));
           Util.option_map v.status_code
             (fun f -> Query.Pair ("StatusCode", (StatusCode.to_query f)));
           Some
             (Query.Pair
                ("Values.member", (DatapointValues.to_query v.values)));
           Some
             (Query.Pair
                ("Timestamps.member", (Timestamps.to_query v.timestamps)));
           Util.option_map v.label
             (fun f -> Query.Pair ("Label", (String.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("messages", (MetricDataResultMessages.to_json v.messages));
           Util.option_map v.status_code
             (fun f -> ("status_code", (StatusCode.to_json f)));
           Some ("values", (DatapointValues.to_json v.values));
           Some ("timestamps", (Timestamps.to_json v.timestamps));
           Util.option_map v.label (fun f -> ("label", (String.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)))])
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        label = (Util.option_map (Json.lookup j "label") String.of_json);
        timestamps =
          (Timestamps.of_json
             (Util.of_option_exn (Json.lookup j "timestamps")));
        values =
          (DatapointValues.of_json
             (Util.of_option_exn (Json.lookup j "values")));
        status_code =
          (Util.option_map (Json.lookup j "status_code") StatusCode.of_json);
        messages =
          (MetricDataResultMessages.of_json
             (Util.of_option_exn (Json.lookup j "messages")))
      }
  end
module DashboardEntry =
  struct
    type t =
      {
      dashboard_name: String.t option ;
      dashboard_arn: String.t option ;
      last_modified: DateTime.t option ;
      size: Long.t option }
    let make ?dashboard_name  ?dashboard_arn  ?last_modified  ?size  () =
      { dashboard_name; dashboard_arn; last_modified; size }
    let parse xml =
      Some
        {
          dashboard_name =
            (Util.option_bind (Xml.member "DashboardName" xml) String.parse);
          dashboard_arn =
            (Util.option_bind (Xml.member "DashboardArn" xml) String.parse);
          last_modified =
            (Util.option_bind (Xml.member "LastModified" xml) DateTime.parse);
          size = (Util.option_bind (Xml.member "Size" xml) Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.size
              (fun f -> Query.Pair ("Size", (Long.to_query f)));
           Util.option_map v.last_modified
             (fun f -> Query.Pair ("LastModified", (DateTime.to_query f)));
           Util.option_map v.dashboard_arn
             (fun f -> Query.Pair ("DashboardArn", (String.to_query f)));
           Util.option_map v.dashboard_name
             (fun f -> Query.Pair ("DashboardName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.size (fun f -> ("size", (Long.to_json f)));
           Util.option_map v.last_modified
             (fun f -> ("last_modified", (DateTime.to_json f)));
           Util.option_map v.dashboard_arn
             (fun f -> ("dashboard_arn", (String.to_json f)));
           Util.option_map v.dashboard_name
             (fun f -> ("dashboard_name", (String.to_json f)))])
    let of_json j =
      {
        dashboard_name =
          (Util.option_map (Json.lookup j "dashboard_name") String.of_json);
        dashboard_arn =
          (Util.option_map (Json.lookup j "dashboard_arn") String.of_json);
        last_modified =
          (Util.option_map (Json.lookup j "last_modified") DateTime.of_json);
        size = (Util.option_map (Json.lookup j "size") Long.of_json)
      }
  end
module InsightRule =
  struct
    type t =
      {
      name: String.t ;
      state: String.t ;
      schema: String.t ;
      definition: String.t }
    let make ~name  ~state  ~schema  ~definition  () =
      { name; state; schema; definition }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          state =
            (Xml.required "State"
               (Util.option_bind (Xml.member "State" xml) String.parse));
          schema =
            (Xml.required "Schema"
               (Util.option_bind (Xml.member "Schema" xml) String.parse));
          definition =
            (Xml.required "Definition"
               (Util.option_bind (Xml.member "Definition" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Definition", (String.to_query v.definition)));
           Some (Query.Pair ("Schema", (String.to_query v.schema)));
           Some (Query.Pair ("State", (String.to_query v.state)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("definition", (String.to_json v.definition));
           Some ("schema", (String.to_json v.schema));
           Some ("state", (String.to_json v.state));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        state = (String.of_json (Util.of_option_exn (Json.lookup j "state")));
        schema =
          (String.of_json (Util.of_option_exn (Json.lookup j "schema")));
        definition =
          (String.of_json (Util.of_option_exn (Json.lookup j "definition")))
      }
  end
module MetricDatum =
  struct
    type t =
      {
      metric_name: String.t ;
      dimensions: Dimensions.t ;
      timestamp: DateTime.t option ;
      value: Double.t option ;
      statistic_values: StatisticSet.t option ;
      values: Values.t ;
      counts: Counts.t ;
      unit: StandardUnit.t option ;
      storage_resolution: Integer.t option }
    let make ~metric_name  ?(dimensions= [])  ?timestamp  ?value 
      ?statistic_values  ?(values= [])  ?(counts= [])  ?unit 
      ?storage_resolution  () =
      {
        metric_name;
        dimensions;
        timestamp;
        value;
        statistic_values;
        values;
        counts;
        unit;
        storage_resolution
      }
    let parse xml =
      Some
        {
          metric_name =
            (Xml.required "MetricName"
               (Util.option_bind (Xml.member "MetricName" xml) String.parse));
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse));
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse);
          value = (Util.option_bind (Xml.member "Value" xml) Double.parse);
          statistic_values =
            (Util.option_bind (Xml.member "StatisticValues" xml)
               StatisticSet.parse);
          values =
            (Util.of_option []
               (Util.option_bind (Xml.member "Values" xml) Values.parse));
          counts =
            (Util.of_option []
               (Util.option_bind (Xml.member "Counts" xml) Counts.parse));
          unit =
            (Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse);
          storage_resolution =
            (Util.option_bind (Xml.member "StorageResolution" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.storage_resolution
              (fun f ->
                 Query.Pair ("StorageResolution", (Integer.to_query f)));
           Util.option_map v.unit
             (fun f -> Query.Pair ("Unit", (StandardUnit.to_query f)));
           Some (Query.Pair ("Counts.member", (Counts.to_query v.counts)));
           Some (Query.Pair ("Values.member", (Values.to_query v.values)));
           Util.option_map v.statistic_values
             (fun f ->
                Query.Pair ("StatisticValues", (StatisticSet.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (Double.to_query f)));
           Util.option_map v.timestamp
             (fun f -> Query.Pair ("Timestamp", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Some (Query.Pair ("MetricName", (String.to_query v.metric_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.storage_resolution
              (fun f -> ("storage_resolution", (Integer.to_json f)));
           Util.option_map v.unit
             (fun f -> ("unit", (StandardUnit.to_json f)));
           Some ("counts", (Counts.to_json v.counts));
           Some ("values", (Values.to_json v.values));
           Util.option_map v.statistic_values
             (fun f -> ("statistic_values", (StatisticSet.to_json f)));
           Util.option_map v.value (fun f -> ("value", (Double.to_json f)));
           Util.option_map v.timestamp
             (fun f -> ("timestamp", (DateTime.to_json f)));
           Some ("dimensions", (Dimensions.to_json v.dimensions));
           Some ("metric_name", (String.to_json v.metric_name))])
    let of_json j =
      {
        metric_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "metric_name")));
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")));
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") DateTime.of_json);
        value = (Util.option_map (Json.lookup j "value") Double.of_json);
        statistic_values =
          (Util.option_map (Json.lookup j "statistic_values")
             StatisticSet.of_json);
        values =
          (Values.of_json (Util.of_option_exn (Json.lookup j "values")));
        counts =
          (Counts.of_json (Util.of_option_exn (Json.lookup j "counts")));
        unit = (Util.option_map (Json.lookup j "unit") StandardUnit.of_json);
        storage_resolution =
          (Util.option_map (Json.lookup j "storage_resolution")
             Integer.of_json)
      }
  end
module Datapoint =
  struct
    type t =
      {
      timestamp: DateTime.t option ;
      sample_count: Double.t option ;
      average: Double.t option ;
      sum: Double.t option ;
      minimum: Double.t option ;
      maximum: Double.t option ;
      unit: StandardUnit.t option ;
      extended_statistics: DatapointValueMap.t option }
    let make ?timestamp  ?sample_count  ?average  ?sum  ?minimum  ?maximum 
      ?unit  ?extended_statistics  () =
      {
        timestamp;
        sample_count;
        average;
        sum;
        minimum;
        maximum;
        unit;
        extended_statistics
      }
    let parse xml =
      Some
        {
          timestamp =
            (Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse);
          sample_count =
            (Util.option_bind (Xml.member "SampleCount" xml) Double.parse);
          average =
            (Util.option_bind (Xml.member "Average" xml) Double.parse);
          sum = (Util.option_bind (Xml.member "Sum" xml) Double.parse);
          minimum =
            (Util.option_bind (Xml.member "Minimum" xml) Double.parse);
          maximum =
            (Util.option_bind (Xml.member "Maximum" xml) Double.parse);
          unit =
            (Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse);
          extended_statistics =
            (Util.option_bind (Xml.member "ExtendedStatistics" xml)
               DatapointValueMap.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.extended_statistics
              (fun f ->
                 Query.Pair
                   ("ExtendedStatistics", (DatapointValueMap.to_query f)));
           Util.option_map v.unit
             (fun f -> Query.Pair ("Unit", (StandardUnit.to_query f)));
           Util.option_map v.maximum
             (fun f -> Query.Pair ("Maximum", (Double.to_query f)));
           Util.option_map v.minimum
             (fun f -> Query.Pair ("Minimum", (Double.to_query f)));
           Util.option_map v.sum
             (fun f -> Query.Pair ("Sum", (Double.to_query f)));
           Util.option_map v.average
             (fun f -> Query.Pair ("Average", (Double.to_query f)));
           Util.option_map v.sample_count
             (fun f -> Query.Pair ("SampleCount", (Double.to_query f)));
           Util.option_map v.timestamp
             (fun f -> Query.Pair ("Timestamp", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.extended_statistics
              (fun f ->
                 ("extended_statistics", (DatapointValueMap.to_json f)));
           Util.option_map v.unit
             (fun f -> ("unit", (StandardUnit.to_json f)));
           Util.option_map v.maximum
             (fun f -> ("maximum", (Double.to_json f)));
           Util.option_map v.minimum
             (fun f -> ("minimum", (Double.to_json f)));
           Util.option_map v.sum (fun f -> ("sum", (Double.to_json f)));
           Util.option_map v.average
             (fun f -> ("average", (Double.to_json f)));
           Util.option_map v.sample_count
             (fun f -> ("sample_count", (Double.to_json f)));
           Util.option_map v.timestamp
             (fun f -> ("timestamp", (DateTime.to_json f)))])
    let of_json j =
      {
        timestamp =
          (Util.option_map (Json.lookup j "timestamp") DateTime.of_json);
        sample_count =
          (Util.option_map (Json.lookup j "sample_count") Double.of_json);
        average = (Util.option_map (Json.lookup j "average") Double.of_json);
        sum = (Util.option_map (Json.lookup j "sum") Double.of_json);
        minimum = (Util.option_map (Json.lookup j "minimum") Double.of_json);
        maximum = (Util.option_map (Json.lookup j "maximum") Double.of_json);
        unit = (Util.option_map (Json.lookup j "unit") StandardUnit.of_json);
        extended_statistics =
          (Util.option_map (Json.lookup j "extended_statistics")
             DatapointValueMap.of_json)
      }
  end
module AlarmHistoryItems =
  struct
    type t = AlarmHistoryItem.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AlarmHistoryItem.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AlarmHistoryItem.to_query v
    let to_json v = `List (List.map AlarmHistoryItem.to_json v)
    let of_json j = Json.to_list AlarmHistoryItem.of_json j
  end
module BatchFailures =
  struct
    type t = PartialFailure.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PartialFailure.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list PartialFailure.to_query v
    let to_json v = `List (List.map PartialFailure.to_json v)
    let of_json j = Json.to_list PartialFailure.of_json j
  end
module DimensionFilters =
  struct
    type t = DimensionFilter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DimensionFilter.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list DimensionFilter.to_query v
    let to_json v = `List (List.map DimensionFilter.to_json v)
    let of_json j = Json.to_list DimensionFilter.of_json j
  end
module RecentlyActive =
  struct
    type t =
      | PT3H 
    let str_to_t = [("PT3H", PT3H)]
    let t_to_str = [(PT3H, "PT3H")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module TagList =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Json.to_list Tag.of_json j
  end
module InsightRuleNames =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module InsightRuleMetricList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module AlarmTypes =
  struct
    type t = AlarmType.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map AlarmType.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AlarmType.to_query v
    let to_json v = `List (List.map AlarmType.to_json v)
    let of_json j = Json.to_list AlarmType.of_json j
  end
module ScanBy =
  struct
    type t =
      | TimestampDescending 
      | TimestampAscending 
    let str_to_t =
      [("TimestampAscending", TimestampAscending);
      ("TimestampDescending", TimestampDescending)]
    let t_to_str =
      [(TimestampAscending, "TimestampAscending");
      (TimestampDescending, "TimestampDescending")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module AlarmNames =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module TagKeyList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module InsightRuleContributorKeyLabels =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module InsightRuleContributors =
  struct
    type t = InsightRuleContributor.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InsightRuleContributor.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list InsightRuleContributor.to_query v
    let to_json v = `List (List.map InsightRuleContributor.to_json v)
    let of_json j = Json.to_list InsightRuleContributor.of_json j
  end
module InsightRuleMetricDatapoints =
  struct
    type t = InsightRuleMetricDatapoint.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InsightRuleMetricDatapoint.parse (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list InsightRuleMetricDatapoint.to_query v
    let to_json v = `List (List.map InsightRuleMetricDatapoint.to_json v)
    let of_json j = Json.to_list InsightRuleMetricDatapoint.of_json j
  end
module DashboardValidationMessages =
  struct
    type t = DashboardValidationMessage.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DashboardValidationMessage.parse (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list DashboardValidationMessage.to_query v
    let to_json v = `List (List.map DashboardValidationMessage.to_json v)
    let of_json j = Json.to_list DashboardValidationMessage.of_json j
  end
module AnomalyDetectors =
  struct
    type t = AnomalyDetector.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AnomalyDetector.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AnomalyDetector.to_query v
    let to_json v = `List (List.map AnomalyDetector.to_json v)
    let of_json j = Json.to_list AnomalyDetector.of_json j
  end
module CompositeAlarms =
  struct
    type t = CompositeAlarm.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CompositeAlarm.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list CompositeAlarm.to_query v
    let to_json v = `List (List.map CompositeAlarm.to_json v)
    let of_json j = Json.to_list CompositeAlarm.of_json j
  end
module MetricAlarms =
  struct
    type t = MetricAlarm.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map MetricAlarm.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list MetricAlarm.to_query v
    let to_json v = `List (List.map MetricAlarm.to_json v)
    let of_json j = Json.to_list MetricAlarm.of_json j
  end
module MetricDataResults =
  struct
    type t = MetricDataResult.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map MetricDataResult.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list MetricDataResult.to_query v
    let to_json v = `List (List.map MetricDataResult.to_json v)
    let of_json j = Json.to_list MetricDataResult.of_json j
  end
module DashboardEntries =
  struct
    type t = DashboardEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DashboardEntry.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list DashboardEntry.to_query v
    let to_json v = `List (List.map DashboardEntry.to_json v)
    let of_json j = Json.to_list DashboardEntry.of_json j
  end
module Metrics =
  struct
    type t = Metric.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Metric.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Metric.to_query v
    let to_json v = `List (List.map Metric.to_json v)
    let of_json j = Json.to_list Metric.of_json j
  end
module InsightRules =
  struct
    type t = InsightRule.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map InsightRule.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list InsightRule.to_query v
    let to_json v = `List (List.map InsightRule.to_json v)
    let of_json j = Json.to_list InsightRule.of_json j
  end
module ExtendedStatistics =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Statistics =
  struct
    type t = Statistic.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Statistic.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Statistic.to_query v
    let to_json v = `List (List.map Statistic.to_json v)
    let of_json j = Json.to_list Statistic.of_json j
  end
module MetricData =
  struct
    type t = MetricDatum.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map MetricDatum.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list MetricDatum.to_query v
    let to_json v = `List (List.map MetricDatum.to_json v)
    let of_json j = Json.to_list MetricDatum.of_json j
  end
module DashboardNames =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Datapoints =
  struct
    type t = Datapoint.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Datapoint.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Datapoint.to_query v
    let to_json v = `List (List.map Datapoint.to_json v)
    let of_json j = Json.to_list Datapoint.of_json j
  end
module DescribeAlarmHistoryOutput =
  struct
    type t =
      {
      alarm_history_items: AlarmHistoryItems.t ;
      next_token: String.t option }
    let make ?(alarm_history_items= [])  ?next_token  () =
      { alarm_history_items; next_token }
    let parse xml =
      Some
        {
          alarm_history_items =
            (Util.of_option []
               (Util.option_bind (Xml.member "AlarmHistoryItems" xml)
                  AlarmHistoryItems.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("AlarmHistoryItems.member",
                  (AlarmHistoryItems.to_query v.alarm_history_items)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("alarm_history_items",
               (AlarmHistoryItems.to_json v.alarm_history_items))])
    let of_json j =
      {
        alarm_history_items =
          (AlarmHistoryItems.of_json
             (Util.of_option_exn (Json.lookup j "alarm_history_items")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DisableInsightRulesOutput =
  struct
    type t = {
      failures: BatchFailures.t }
    let make ?(failures= [])  () = { failures }
    let parse xml =
      Some
        {
          failures =
            (Util.of_option []
               (Util.option_bind (Xml.member "Failures" xml)
                  BatchFailures.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Failures.member", (BatchFailures.to_query v.failures)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("failures", (BatchFailures.to_json v.failures))])
    let of_json j =
      {
        failures =
          (BatchFailures.of_json
             (Util.of_option_exn (Json.lookup j "failures")))
      }
  end
module GetDashboardOutput =
  struct
    type t =
      {
      dashboard_arn: String.t option ;
      dashboard_body: String.t option ;
      dashboard_name: String.t option }
    let make ?dashboard_arn  ?dashboard_body  ?dashboard_name  () =
      { dashboard_arn; dashboard_body; dashboard_name }
    let parse xml =
      Some
        {
          dashboard_arn =
            (Util.option_bind (Xml.member "DashboardArn" xml) String.parse);
          dashboard_body =
            (Util.option_bind (Xml.member "DashboardBody" xml) String.parse);
          dashboard_name =
            (Util.option_bind (Xml.member "DashboardName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.dashboard_name
              (fun f -> Query.Pair ("DashboardName", (String.to_query f)));
           Util.option_map v.dashboard_body
             (fun f -> Query.Pair ("DashboardBody", (String.to_query f)));
           Util.option_map v.dashboard_arn
             (fun f -> Query.Pair ("DashboardArn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.dashboard_name
              (fun f -> ("dashboard_name", (String.to_json f)));
           Util.option_map v.dashboard_body
             (fun f -> ("dashboard_body", (String.to_json f)));
           Util.option_map v.dashboard_arn
             (fun f -> ("dashboard_arn", (String.to_json f)))])
    let of_json j =
      {
        dashboard_arn =
          (Util.option_map (Json.lookup j "dashboard_arn") String.of_json);
        dashboard_body =
          (Util.option_map (Json.lookup j "dashboard_body") String.of_json);
        dashboard_name =
          (Util.option_map (Json.lookup j "dashboard_name") String.of_json)
      }
  end
module GetDashboardInput =
  struct
    type t = {
      dashboard_name: String.t }
    let make ~dashboard_name  () = { dashboard_name }
    let parse xml =
      Some
        {
          dashboard_name =
            (Xml.required "DashboardName"
               (Util.option_bind (Xml.member "DashboardName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DashboardName", (String.to_query v.dashboard_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("dashboard_name", (String.to_json v.dashboard_name))])
    let of_json j =
      {
        dashboard_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "dashboard_name")))
      }
  end
module ListMetricsInput =
  struct
    type t =
      {
      namespace: String.t option ;
      metric_name: String.t option ;
      dimensions: DimensionFilters.t ;
      next_token: String.t option ;
      recently_active: RecentlyActive.t option }
    let make ?namespace  ?metric_name  ?(dimensions= [])  ?next_token 
      ?recently_active  () =
      { namespace; metric_name; dimensions; next_token; recently_active }
    let parse xml =
      Some
        {
          namespace =
            (Util.option_bind (Xml.member "Namespace" xml) String.parse);
          metric_name =
            (Util.option_bind (Xml.member "MetricName" xml) String.parse);
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  DimensionFilters.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          recently_active =
            (Util.option_bind (Xml.member "RecentlyActive" xml)
               RecentlyActive.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.recently_active
              (fun f ->
                 Query.Pair ("RecentlyActive", (RecentlyActive.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("Dimensions.member",
                  (DimensionFilters.to_query v.dimensions)));
           Util.option_map v.metric_name
             (fun f -> Query.Pair ("MetricName", (String.to_query f)));
           Util.option_map v.namespace
             (fun f -> Query.Pair ("Namespace", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.recently_active
              (fun f -> ("recently_active", (RecentlyActive.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("dimensions", (DimensionFilters.to_json v.dimensions));
           Util.option_map v.metric_name
             (fun f -> ("metric_name", (String.to_json f)));
           Util.option_map v.namespace
             (fun f -> ("namespace", (String.to_json f)))])
    let of_json j =
      {
        namespace =
          (Util.option_map (Json.lookup j "namespace") String.of_json);
        metric_name =
          (Util.option_map (Json.lookup j "metric_name") String.of_json);
        dimensions =
          (DimensionFilters.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        recently_active =
          (Util.option_map (Json.lookup j "recently_active")
             RecentlyActive.of_json)
      }
  end
module TagResourceInput =
  struct
    type t = {
      resource_a_r_n: String.t ;
      tags: TagList.t }
    let make ~resource_a_r_n  ~tags  () = { resource_a_r_n; tags }
    let parse xml =
      Some
        {
          resource_a_r_n =
            (Xml.required "ResourceARN"
               (Util.option_bind (Xml.member "ResourceARN" xml) String.parse));
          tags =
            (Xml.required "Tags"
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair ("ResourceARN", (String.to_query v.resource_a_r_n)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some ("resource_a_r_n", (String.to_json v.resource_a_r_n))])
    let of_json j =
      {
        resource_a_r_n =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_a_r_n")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module InternalServiceFault =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ConcurrentModificationException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module EnableInsightRulesInput =
  struct
    type t = {
      rule_names: InsightRuleNames.t }
    let make ~rule_names  () = { rule_names }
    let parse xml =
      Some
        {
          rule_names =
            (Xml.required "RuleNames"
               (Util.option_bind (Xml.member "RuleNames" xml)
                  InsightRuleNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RuleNames.member",
                   (InsightRuleNames.to_query v.rule_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("rule_names", (InsightRuleNames.to_json v.rule_names))])
    let of_json j =
      {
        rule_names =
          (InsightRuleNames.of_json
             (Util.of_option_exn (Json.lookup j "rule_names")))
      }
  end
module GetInsightRuleReportInput =
  struct
    type t =
      {
      rule_name: String.t ;
      start_time: DateTime.t ;
      end_time: DateTime.t ;
      period: Integer.t ;
      max_contributor_count: Integer.t option ;
      metrics: InsightRuleMetricList.t ;
      order_by: String.t option }
    let make ~rule_name  ~start_time  ~end_time  ~period 
      ?max_contributor_count  ?(metrics= [])  ?order_by  () =
      {
        rule_name;
        start_time;
        end_time;
        period;
        max_contributor_count;
        metrics;
        order_by
      }
    let parse xml =
      Some
        {
          rule_name =
            (Xml.required "RuleName"
               (Util.option_bind (Xml.member "RuleName" xml) String.parse));
          start_time =
            (Xml.required "StartTime"
               (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse));
          end_time =
            (Xml.required "EndTime"
               (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse));
          period =
            (Xml.required "Period"
               (Util.option_bind (Xml.member "Period" xml) Integer.parse));
          max_contributor_count =
            (Util.option_bind (Xml.member "MaxContributorCount" xml)
               Integer.parse);
          metrics =
            (Util.of_option []
               (Util.option_bind (Xml.member "Metrics" xml)
                  InsightRuleMetricList.parse));
          order_by =
            (Util.option_bind (Xml.member "OrderBy" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.order_by
              (fun f -> Query.Pair ("OrderBy", (String.to_query f)));
           Some
             (Query.Pair
                ("Metrics.member",
                  (InsightRuleMetricList.to_query v.metrics)));
           Util.option_map v.max_contributor_count
             (fun f ->
                Query.Pair ("MaxContributorCount", (Integer.to_query f)));
           Some (Query.Pair ("Period", (Integer.to_query v.period)));
           Some (Query.Pair ("EndTime", (DateTime.to_query v.end_time)));
           Some (Query.Pair ("StartTime", (DateTime.to_query v.start_time)));
           Some (Query.Pair ("RuleName", (String.to_query v.rule_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.order_by
              (fun f -> ("order_by", (String.to_json f)));
           Some ("metrics", (InsightRuleMetricList.to_json v.metrics));
           Util.option_map v.max_contributor_count
             (fun f -> ("max_contributor_count", (Integer.to_json f)));
           Some ("period", (Integer.to_json v.period));
           Some ("end_time", (DateTime.to_json v.end_time));
           Some ("start_time", (DateTime.to_json v.start_time));
           Some ("rule_name", (String.to_json v.rule_name))])
    let of_json j =
      {
        rule_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "rule_name")));
        start_time =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "start_time")));
        end_time =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "end_time")));
        period =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "period")));
        max_contributor_count =
          (Util.option_map (Json.lookup j "max_contributor_count")
             Integer.of_json);
        metrics =
          (InsightRuleMetricList.of_json
             (Util.of_option_exn (Json.lookup j "metrics")));
        order_by =
          (Util.option_map (Json.lookup j "order_by") String.of_json)
      }
  end
module DescribeAlarmHistoryInput =
  struct
    type t =
      {
      alarm_name: String.t option ;
      alarm_types: AlarmTypes.t ;
      history_item_type: HistoryItemType.t option ;
      start_date: DateTime.t option ;
      end_date: DateTime.t option ;
      max_records: Integer.t option ;
      next_token: String.t option ;
      scan_by: ScanBy.t option }
    let make ?alarm_name  ?(alarm_types= [])  ?history_item_type  ?start_date
       ?end_date  ?max_records  ?next_token  ?scan_by  () =
      {
        alarm_name;
        alarm_types;
        history_item_type;
        start_date;
        end_date;
        max_records;
        next_token;
        scan_by
      }
    let parse xml =
      Some
        {
          alarm_name =
            (Util.option_bind (Xml.member "AlarmName" xml) String.parse);
          alarm_types =
            (Util.of_option []
               (Util.option_bind (Xml.member "AlarmTypes" xml)
                  AlarmTypes.parse));
          history_item_type =
            (Util.option_bind (Xml.member "HistoryItemType" xml)
               HistoryItemType.parse);
          start_date =
            (Util.option_bind (Xml.member "StartDate" xml) DateTime.parse);
          end_date =
            (Util.option_bind (Xml.member "EndDate" xml) DateTime.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          scan_by = (Util.option_bind (Xml.member "ScanBy" xml) ScanBy.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.scan_by
              (fun f -> Query.Pair ("ScanBy", (ScanBy.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.end_date
             (fun f -> Query.Pair ("EndDate", (DateTime.to_query f)));
           Util.option_map v.start_date
             (fun f -> Query.Pair ("StartDate", (DateTime.to_query f)));
           Util.option_map v.history_item_type
             (fun f ->
                Query.Pair ("HistoryItemType", (HistoryItemType.to_query f)));
           Some
             (Query.Pair
                ("AlarmTypes.member", (AlarmTypes.to_query v.alarm_types)));
           Util.option_map v.alarm_name
             (fun f -> Query.Pair ("AlarmName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.scan_by
              (fun f -> ("scan_by", (ScanBy.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.end_date
             (fun f -> ("end_date", (DateTime.to_json f)));
           Util.option_map v.start_date
             (fun f -> ("start_date", (DateTime.to_json f)));
           Util.option_map v.history_item_type
             (fun f -> ("history_item_type", (HistoryItemType.to_json f)));
           Some ("alarm_types", (AlarmTypes.to_json v.alarm_types));
           Util.option_map v.alarm_name
             (fun f -> ("alarm_name", (String.to_json f)))])
    let of_json j =
      {
        alarm_name =
          (Util.option_map (Json.lookup j "alarm_name") String.of_json);
        alarm_types =
          (AlarmTypes.of_json
             (Util.of_option_exn (Json.lookup j "alarm_types")));
        history_item_type =
          (Util.option_map (Json.lookup j "history_item_type")
             HistoryItemType.of_json);
        start_date =
          (Util.option_map (Json.lookup j "start_date") DateTime.of_json);
        end_date =
          (Util.option_map (Json.lookup j "end_date") DateTime.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        scan_by = (Util.option_map (Json.lookup j "scan_by") ScanBy.of_json)
      }
  end
module DescribeAlarmsInput =
  struct
    type t =
      {
      alarm_names: AlarmNames.t ;
      alarm_name_prefix: String.t option ;
      alarm_types: AlarmTypes.t ;
      children_of_alarm_name: String.t option ;
      parents_of_alarm_name: String.t option ;
      state_value: StateValue.t option ;
      action_prefix: String.t option ;
      max_records: Integer.t option ;
      next_token: String.t option }
    let make ?(alarm_names= [])  ?alarm_name_prefix  ?(alarm_types= []) 
      ?children_of_alarm_name  ?parents_of_alarm_name  ?state_value 
      ?action_prefix  ?max_records  ?next_token  () =
      {
        alarm_names;
        alarm_name_prefix;
        alarm_types;
        children_of_alarm_name;
        parents_of_alarm_name;
        state_value;
        action_prefix;
        max_records;
        next_token
      }
    let parse xml =
      Some
        {
          alarm_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "AlarmNames" xml)
                  AlarmNames.parse));
          alarm_name_prefix =
            (Util.option_bind (Xml.member "AlarmNamePrefix" xml) String.parse);
          alarm_types =
            (Util.of_option []
               (Util.option_bind (Xml.member "AlarmTypes" xml)
                  AlarmTypes.parse));
          children_of_alarm_name =
            (Util.option_bind (Xml.member "ChildrenOfAlarmName" xml)
               String.parse);
          parents_of_alarm_name =
            (Util.option_bind (Xml.member "ParentsOfAlarmName" xml)
               String.parse);
          state_value =
            (Util.option_bind (Xml.member "StateValue" xml) StateValue.parse);
          action_prefix =
            (Util.option_bind (Xml.member "ActionPrefix" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.action_prefix
             (fun f -> Query.Pair ("ActionPrefix", (String.to_query f)));
           Util.option_map v.state_value
             (fun f -> Query.Pair ("StateValue", (StateValue.to_query f)));
           Util.option_map v.parents_of_alarm_name
             (fun f -> Query.Pair ("ParentsOfAlarmName", (String.to_query f)));
           Util.option_map v.children_of_alarm_name
             (fun f ->
                Query.Pair ("ChildrenOfAlarmName", (String.to_query f)));
           Some
             (Query.Pair
                ("AlarmTypes.member", (AlarmTypes.to_query v.alarm_types)));
           Util.option_map v.alarm_name_prefix
             (fun f -> Query.Pair ("AlarmNamePrefix", (String.to_query f)));
           Some
             (Query.Pair
                ("AlarmNames.member", (AlarmNames.to_query v.alarm_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.action_prefix
             (fun f -> ("action_prefix", (String.to_json f)));
           Util.option_map v.state_value
             (fun f -> ("state_value", (StateValue.to_json f)));
           Util.option_map v.parents_of_alarm_name
             (fun f -> ("parents_of_alarm_name", (String.to_json f)));
           Util.option_map v.children_of_alarm_name
             (fun f -> ("children_of_alarm_name", (String.to_json f)));
           Some ("alarm_types", (AlarmTypes.to_json v.alarm_types));
           Util.option_map v.alarm_name_prefix
             (fun f -> ("alarm_name_prefix", (String.to_json f)));
           Some ("alarm_names", (AlarmNames.to_json v.alarm_names))])
    let of_json j =
      {
        alarm_names =
          (AlarmNames.of_json
             (Util.of_option_exn (Json.lookup j "alarm_names")));
        alarm_name_prefix =
          (Util.option_map (Json.lookup j "alarm_name_prefix") String.of_json);
        alarm_types =
          (AlarmTypes.of_json
             (Util.of_option_exn (Json.lookup j "alarm_types")));
        children_of_alarm_name =
          (Util.option_map (Json.lookup j "children_of_alarm_name")
             String.of_json);
        parents_of_alarm_name =
          (Util.option_map (Json.lookup j "parents_of_alarm_name")
             String.of_json);
        state_value =
          (Util.option_map (Json.lookup j "state_value") StateValue.of_json);
        action_prefix =
          (Util.option_map (Json.lookup j "action_prefix") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListTagsForResourceInput =
  struct
    type t = {
      resource_a_r_n: String.t }
    let make ~resource_a_r_n  () = { resource_a_r_n }
    let parse xml =
      Some
        {
          resource_a_r_n =
            (Xml.required "ResourceARN"
               (Util.option_bind (Xml.member "ResourceARN" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("ResourceARN", (String.to_query v.resource_a_r_n)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("resource_a_r_n", (String.to_json v.resource_a_r_n))])
    let of_json j =
      {
        resource_a_r_n =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_a_r_n")))
      }
  end
module DisableInsightRulesInput =
  struct
    type t = {
      rule_names: InsightRuleNames.t }
    let make ~rule_names  () = { rule_names }
    let parse xml =
      Some
        {
          rule_names =
            (Xml.required "RuleNames"
               (Util.option_bind (Xml.member "RuleNames" xml)
                  InsightRuleNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RuleNames.member",
                   (InsightRuleNames.to_query v.rule_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("rule_names", (InsightRuleNames.to_json v.rule_names))])
    let of_json j =
      {
        rule_names =
          (InsightRuleNames.of_json
             (Util.of_option_exn (Json.lookup j "rule_names")))
      }
  end
module UntagResourceInput =
  struct
    type t = {
      resource_a_r_n: String.t ;
      tag_keys: TagKeyList.t }
    let make ~resource_a_r_n  ~tag_keys  () = { resource_a_r_n; tag_keys }
    let parse xml =
      Some
        {
          resource_a_r_n =
            (Xml.required "ResourceARN"
               (Util.option_bind (Xml.member "ResourceARN" xml) String.parse));
          tag_keys =
            (Xml.required "TagKeys"
               (Util.option_bind (Xml.member "TagKeys" xml) TagKeyList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TagKeys.member", (TagKeyList.to_query v.tag_keys)));
           Some
             (Query.Pair ("ResourceARN", (String.to_query v.resource_a_r_n)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tag_keys", (TagKeyList.to_json v.tag_keys));
           Some ("resource_a_r_n", (String.to_json v.resource_a_r_n))])
    let of_json j =
      {
        resource_a_r_n =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_a_r_n")));
        tag_keys =
          (TagKeyList.of_json (Util.of_option_exn (Json.lookup j "tag_keys")))
      }
  end
module GetMetricDataInput =
  struct
    type t =
      {
      metric_data_queries: MetricDataQueries.t ;
      start_time: DateTime.t ;
      end_time: DateTime.t ;
      next_token: String.t option ;
      scan_by: ScanBy.t option ;
      max_datapoints: Integer.t option }
    let make ~metric_data_queries  ~start_time  ~end_time  ?next_token 
      ?scan_by  ?max_datapoints  () =
      {
        metric_data_queries;
        start_time;
        end_time;
        next_token;
        scan_by;
        max_datapoints
      }
    let parse xml =
      Some
        {
          metric_data_queries =
            (Xml.required "MetricDataQueries"
               (Util.option_bind (Xml.member "MetricDataQueries" xml)
                  MetricDataQueries.parse));
          start_time =
            (Xml.required "StartTime"
               (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse));
          end_time =
            (Xml.required "EndTime"
               (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          scan_by = (Util.option_bind (Xml.member "ScanBy" xml) ScanBy.parse);
          max_datapoints =
            (Util.option_bind (Xml.member "MaxDatapoints" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_datapoints
              (fun f -> Query.Pair ("MaxDatapoints", (Integer.to_query f)));
           Util.option_map v.scan_by
             (fun f -> Query.Pair ("ScanBy", (ScanBy.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("EndTime", (DateTime.to_query v.end_time)));
           Some (Query.Pair ("StartTime", (DateTime.to_query v.start_time)));
           Some
             (Query.Pair
                ("MetricDataQueries.member",
                  (MetricDataQueries.to_query v.metric_data_queries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_datapoints
              (fun f -> ("max_datapoints", (Integer.to_json f)));
           Util.option_map v.scan_by
             (fun f -> ("scan_by", (ScanBy.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("end_time", (DateTime.to_json v.end_time));
           Some ("start_time", (DateTime.to_json v.start_time));
           Some
             ("metric_data_queries",
               (MetricDataQueries.to_json v.metric_data_queries))])
    let of_json j =
      {
        metric_data_queries =
          (MetricDataQueries.of_json
             (Util.of_option_exn (Json.lookup j "metric_data_queries")));
        start_time =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "start_time")));
        end_time =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "end_time")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        scan_by = (Util.option_map (Json.lookup j "scan_by") ScanBy.of_json);
        max_datapoints =
          (Util.option_map (Json.lookup j "max_datapoints") Integer.of_json)
      }
  end
module UntagResourceOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetMetricWidgetImageInput =
  struct
    type t = {
      metric_widget: String.t ;
      output_format: String.t option }
    let make ~metric_widget  ?output_format  () =
      { metric_widget; output_format }
    let parse xml =
      Some
        {
          metric_widget =
            (Xml.required "MetricWidget"
               (Util.option_bind (Xml.member "MetricWidget" xml) String.parse));
          output_format =
            (Util.option_bind (Xml.member "OutputFormat" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.output_format
              (fun f -> Query.Pair ("OutputFormat", (String.to_query f)));
           Some
             (Query.Pair ("MetricWidget", (String.to_query v.metric_widget)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.output_format
              (fun f -> ("output_format", (String.to_json f)));
           Some ("metric_widget", (String.to_json v.metric_widget))])
    let of_json j =
      {
        metric_widget =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "metric_widget")));
        output_format =
          (Util.option_map (Json.lookup j "output_format") String.of_json)
      }
  end
module EnableInsightRulesOutput =
  struct
    type t = {
      failures: BatchFailures.t }
    let make ?(failures= [])  () = { failures }
    let parse xml =
      Some
        {
          failures =
            (Util.of_option []
               (Util.option_bind (Xml.member "Failures" xml)
                  BatchFailures.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Failures.member", (BatchFailures.to_query v.failures)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("failures", (BatchFailures.to_json v.failures))])
    let of_json j =
      {
        failures =
          (BatchFailures.of_json
             (Util.of_option_exn (Json.lookup j "failures")))
      }
  end
module LimitExceededFault =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module PutMetricAlarmInput =
  struct
    type t =
      {
      alarm_name: String.t ;
      alarm_description: String.t option ;
      actions_enabled: Boolean.t option ;
      o_k_actions: ResourceList.t ;
      alarm_actions: ResourceList.t ;
      insufficient_data_actions: ResourceList.t ;
      metric_name: String.t option ;
      namespace: String.t option ;
      statistic: Statistic.t option ;
      extended_statistic: String.t option ;
      dimensions: Dimensions.t ;
      period: Integer.t option ;
      unit: StandardUnit.t option ;
      evaluation_periods: Integer.t ;
      datapoints_to_alarm: Integer.t option ;
      threshold: Double.t option ;
      comparison_operator: ComparisonOperator.t ;
      treat_missing_data: String.t option ;
      evaluate_low_sample_count_percentile: String.t option ;
      metrics: MetricDataQueries.t ;
      tags: TagList.t ;
      threshold_metric_id: String.t option }
    let make ~alarm_name  ?alarm_description  ?actions_enabled 
      ?(o_k_actions= [])  ?(alarm_actions= [])  ?(insufficient_data_actions=
      [])  ?metric_name  ?namespace  ?statistic  ?extended_statistic 
      ?(dimensions= [])  ?period  ?unit  ~evaluation_periods 
      ?datapoints_to_alarm  ?threshold  ~comparison_operator 
      ?treat_missing_data  ?evaluate_low_sample_count_percentile  ?(metrics=
      [])  ?(tags= [])  ?threshold_metric_id  () =
      {
        alarm_name;
        alarm_description;
        actions_enabled;
        o_k_actions;
        alarm_actions;
        insufficient_data_actions;
        metric_name;
        namespace;
        statistic;
        extended_statistic;
        dimensions;
        period;
        unit;
        evaluation_periods;
        datapoints_to_alarm;
        threshold;
        comparison_operator;
        treat_missing_data;
        evaluate_low_sample_count_percentile;
        metrics;
        tags;
        threshold_metric_id
      }
    let parse xml =
      Some
        {
          alarm_name =
            (Xml.required "AlarmName"
               (Util.option_bind (Xml.member "AlarmName" xml) String.parse));
          alarm_description =
            (Util.option_bind (Xml.member "AlarmDescription" xml)
               String.parse);
          actions_enabled =
            (Util.option_bind (Xml.member "ActionsEnabled" xml) Boolean.parse);
          o_k_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "OKActions" xml)
                  ResourceList.parse));
          alarm_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "AlarmActions" xml)
                  ResourceList.parse));
          insufficient_data_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "InsufficientDataActions" xml)
                  ResourceList.parse));
          metric_name =
            (Util.option_bind (Xml.member "MetricName" xml) String.parse);
          namespace =
            (Util.option_bind (Xml.member "Namespace" xml) String.parse);
          statistic =
            (Util.option_bind (Xml.member "Statistic" xml) Statistic.parse);
          extended_statistic =
            (Util.option_bind (Xml.member "ExtendedStatistic" xml)
               String.parse);
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse));
          period = (Util.option_bind (Xml.member "Period" xml) Integer.parse);
          unit =
            (Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse);
          evaluation_periods =
            (Xml.required "EvaluationPeriods"
               (Util.option_bind (Xml.member "EvaluationPeriods" xml)
                  Integer.parse));
          datapoints_to_alarm =
            (Util.option_bind (Xml.member "DatapointsToAlarm" xml)
               Integer.parse);
          threshold =
            (Util.option_bind (Xml.member "Threshold" xml) Double.parse);
          comparison_operator =
            (Xml.required "ComparisonOperator"
               (Util.option_bind (Xml.member "ComparisonOperator" xml)
                  ComparisonOperator.parse));
          treat_missing_data =
            (Util.option_bind (Xml.member "TreatMissingData" xml)
               String.parse);
          evaluate_low_sample_count_percentile =
            (Util.option_bind
               (Xml.member "EvaluateLowSampleCountPercentile" xml)
               String.parse);
          metrics =
            (Util.of_option []
               (Util.option_bind (Xml.member "Metrics" xml)
                  MetricDataQueries.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse));
          threshold_metric_id =
            (Util.option_bind (Xml.member "ThresholdMetricId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.threshold_metric_id
              (fun f -> Query.Pair ("ThresholdMetricId", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("Metrics.member", (MetricDataQueries.to_query v.metrics)));
           Util.option_map v.evaluate_low_sample_count_percentile
             (fun f ->
                Query.Pair
                  ("EvaluateLowSampleCountPercentile", (String.to_query f)));
           Util.option_map v.treat_missing_data
             (fun f -> Query.Pair ("TreatMissingData", (String.to_query f)));
           Some
             (Query.Pair
                ("ComparisonOperator",
                  (ComparisonOperator.to_query v.comparison_operator)));
           Util.option_map v.threshold
             (fun f -> Query.Pair ("Threshold", (Double.to_query f)));
           Util.option_map v.datapoints_to_alarm
             (fun f -> Query.Pair ("DatapointsToAlarm", (Integer.to_query f)));
           Some
             (Query.Pair
                ("EvaluationPeriods",
                  (Integer.to_query v.evaluation_periods)));
           Util.option_map v.unit
             (fun f -> Query.Pair ("Unit", (StandardUnit.to_query f)));
           Util.option_map v.period
             (fun f -> Query.Pair ("Period", (Integer.to_query f)));
           Some
             (Query.Pair
                ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Util.option_map v.extended_statistic
             (fun f -> Query.Pair ("ExtendedStatistic", (String.to_query f)));
           Util.option_map v.statistic
             (fun f -> Query.Pair ("Statistic", (Statistic.to_query f)));
           Util.option_map v.namespace
             (fun f -> Query.Pair ("Namespace", (String.to_query f)));
           Util.option_map v.metric_name
             (fun f -> Query.Pair ("MetricName", (String.to_query f)));
           Some
             (Query.Pair
                ("InsufficientDataActions.member",
                  (ResourceList.to_query v.insufficient_data_actions)));
           Some
             (Query.Pair
                ("AlarmActions.member",
                  (ResourceList.to_query v.alarm_actions)));
           Some
             (Query.Pair
                ("OKActions.member", (ResourceList.to_query v.o_k_actions)));
           Util.option_map v.actions_enabled
             (fun f -> Query.Pair ("ActionsEnabled", (Boolean.to_query f)));
           Util.option_map v.alarm_description
             (fun f -> Query.Pair ("AlarmDescription", (String.to_query f)));
           Some (Query.Pair ("AlarmName", (String.to_query v.alarm_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.threshold_metric_id
              (fun f -> ("threshold_metric_id", (String.to_json f)));
           Some ("tags", (TagList.to_json v.tags));
           Some ("metrics", (MetricDataQueries.to_json v.metrics));
           Util.option_map v.evaluate_low_sample_count_percentile
             (fun f ->
                ("evaluate_low_sample_count_percentile", (String.to_json f)));
           Util.option_map v.treat_missing_data
             (fun f -> ("treat_missing_data", (String.to_json f)));
           Some
             ("comparison_operator",
               (ComparisonOperator.to_json v.comparison_operator));
           Util.option_map v.threshold
             (fun f -> ("threshold", (Double.to_json f)));
           Util.option_map v.datapoints_to_alarm
             (fun f -> ("datapoints_to_alarm", (Integer.to_json f)));
           Some
             ("evaluation_periods", (Integer.to_json v.evaluation_periods));
           Util.option_map v.unit
             (fun f -> ("unit", (StandardUnit.to_json f)));
           Util.option_map v.period
             (fun f -> ("period", (Integer.to_json f)));
           Some ("dimensions", (Dimensions.to_json v.dimensions));
           Util.option_map v.extended_statistic
             (fun f -> ("extended_statistic", (String.to_json f)));
           Util.option_map v.statistic
             (fun f -> ("statistic", (Statistic.to_json f)));
           Util.option_map v.namespace
             (fun f -> ("namespace", (String.to_json f)));
           Util.option_map v.metric_name
             (fun f -> ("metric_name", (String.to_json f)));
           Some
             ("insufficient_data_actions",
               (ResourceList.to_json v.insufficient_data_actions));
           Some ("alarm_actions", (ResourceList.to_json v.alarm_actions));
           Some ("o_k_actions", (ResourceList.to_json v.o_k_actions));
           Util.option_map v.actions_enabled
             (fun f -> ("actions_enabled", (Boolean.to_json f)));
           Util.option_map v.alarm_description
             (fun f -> ("alarm_description", (String.to_json f)));
           Some ("alarm_name", (String.to_json v.alarm_name))])
    let of_json j =
      {
        alarm_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "alarm_name")));
        alarm_description =
          (Util.option_map (Json.lookup j "alarm_description") String.of_json);
        actions_enabled =
          (Util.option_map (Json.lookup j "actions_enabled") Boolean.of_json);
        o_k_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "o_k_actions")));
        alarm_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "alarm_actions")));
        insufficient_data_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "insufficient_data_actions")));
        metric_name =
          (Util.option_map (Json.lookup j "metric_name") String.of_json);
        namespace =
          (Util.option_map (Json.lookup j "namespace") String.of_json);
        statistic =
          (Util.option_map (Json.lookup j "statistic") Statistic.of_json);
        extended_statistic =
          (Util.option_map (Json.lookup j "extended_statistic")
             String.of_json);
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")));
        period = (Util.option_map (Json.lookup j "period") Integer.of_json);
        unit = (Util.option_map (Json.lookup j "unit") StandardUnit.of_json);
        evaluation_periods =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "evaluation_periods")));
        datapoints_to_alarm =
          (Util.option_map (Json.lookup j "datapoints_to_alarm")
             Integer.of_json);
        threshold =
          (Util.option_map (Json.lookup j "threshold") Double.of_json);
        comparison_operator =
          (ComparisonOperator.of_json
             (Util.of_option_exn (Json.lookup j "comparison_operator")));
        treat_missing_data =
          (Util.option_map (Json.lookup j "treat_missing_data")
             String.of_json);
        evaluate_low_sample_count_percentile =
          (Util.option_map
             (Json.lookup j "evaluate_low_sample_count_percentile")
             String.of_json);
        metrics =
          (MetricDataQueries.of_json
             (Util.of_option_exn (Json.lookup j "metrics")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")));
        threshold_metric_id =
          (Util.option_map (Json.lookup j "threshold_metric_id")
             String.of_json)
      }
  end
module SetAlarmStateInput =
  struct
    type t =
      {
      alarm_name: String.t ;
      state_value: StateValue.t ;
      state_reason: String.t ;
      state_reason_data: String.t option }
    let make ~alarm_name  ~state_value  ~state_reason  ?state_reason_data  ()
      = { alarm_name; state_value; state_reason; state_reason_data }
    let parse xml =
      Some
        {
          alarm_name =
            (Xml.required "AlarmName"
               (Util.option_bind (Xml.member "AlarmName" xml) String.parse));
          state_value =
            (Xml.required "StateValue"
               (Util.option_bind (Xml.member "StateValue" xml)
                  StateValue.parse));
          state_reason =
            (Xml.required "StateReason"
               (Util.option_bind (Xml.member "StateReason" xml) String.parse));
          state_reason_data =
            (Util.option_bind (Xml.member "StateReasonData" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.state_reason_data
              (fun f -> Query.Pair ("StateReasonData", (String.to_query f)));
           Some
             (Query.Pair ("StateReason", (String.to_query v.state_reason)));
           Some
             (Query.Pair ("StateValue", (StateValue.to_query v.state_value)));
           Some (Query.Pair ("AlarmName", (String.to_query v.alarm_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.state_reason_data
              (fun f -> ("state_reason_data", (String.to_json f)));
           Some ("state_reason", (String.to_json v.state_reason));
           Some ("state_value", (StateValue.to_json v.state_value));
           Some ("alarm_name", (String.to_json v.alarm_name))])
    let of_json j =
      {
        alarm_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "alarm_name")));
        state_value =
          (StateValue.of_json
             (Util.of_option_exn (Json.lookup j "state_value")));
        state_reason =
          (String.of_json (Util.of_option_exn (Json.lookup j "state_reason")));
        state_reason_data =
          (Util.option_map (Json.lookup j "state_reason_data") String.of_json)
      }
  end
module EnableAlarmActionsInput =
  struct
    type t = {
      alarm_names: AlarmNames.t }
    let make ~alarm_names  () = { alarm_names }
    let parse xml =
      Some
        {
          alarm_names =
            (Xml.required "AlarmNames"
               (Util.option_bind (Xml.member "AlarmNames" xml)
                  AlarmNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AlarmNames.member", (AlarmNames.to_query v.alarm_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("alarm_names", (AlarmNames.to_json v.alarm_names))])
    let of_json j =
      {
        alarm_names =
          (AlarmNames.of_json
             (Util.of_option_exn (Json.lookup j "alarm_names")))
      }
  end
module ListDashboardsInput =
  struct
    type t =
      {
      dashboard_name_prefix: String.t option ;
      next_token: String.t option }
    let make ?dashboard_name_prefix  ?next_token  () =
      { dashboard_name_prefix; next_token }
    let parse xml =
      Some
        {
          dashboard_name_prefix =
            (Util.option_bind (Xml.member "DashboardNamePrefix" xml)
               String.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.dashboard_name_prefix
             (fun f ->
                Query.Pair ("DashboardNamePrefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.dashboard_name_prefix
             (fun f -> ("dashboard_name_prefix", (String.to_json f)))])
    let of_json j =
      {
        dashboard_name_prefix =
          (Util.option_map (Json.lookup j "dashboard_name_prefix")
             String.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module GetMetricWidgetImageOutput =
  struct
    type t = {
      metric_widget_image: Blob.t option }
    let make ?metric_widget_image  () = { metric_widget_image }
    let parse xml =
      Some
        {
          metric_widget_image =
            (Util.option_bind (Xml.member "MetricWidgetImage" xml) Blob.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.metric_widget_image
              (fun f -> Query.Pair ("MetricWidgetImage", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.metric_widget_image
              (fun f -> ("metric_widget_image", (Blob.to_json f)))])
    let of_json j =
      {
        metric_widget_image =
          (Util.option_map (Json.lookup j "metric_widget_image") Blob.of_json)
      }
  end
module DescribeAlarmsForMetricInput =
  struct
    type t =
      {
      metric_name: String.t ;
      namespace: String.t ;
      statistic: Statistic.t option ;
      extended_statistic: String.t option ;
      dimensions: Dimensions.t ;
      period: Integer.t option ;
      unit: StandardUnit.t option }
    let make ~metric_name  ~namespace  ?statistic  ?extended_statistic 
      ?(dimensions= [])  ?period  ?unit  () =
      {
        metric_name;
        namespace;
        statistic;
        extended_statistic;
        dimensions;
        period;
        unit
      }
    let parse xml =
      Some
        {
          metric_name =
            (Xml.required "MetricName"
               (Util.option_bind (Xml.member "MetricName" xml) String.parse));
          namespace =
            (Xml.required "Namespace"
               (Util.option_bind (Xml.member "Namespace" xml) String.parse));
          statistic =
            (Util.option_bind (Xml.member "Statistic" xml) Statistic.parse);
          extended_statistic =
            (Util.option_bind (Xml.member "ExtendedStatistic" xml)
               String.parse);
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse));
          period = (Util.option_bind (Xml.member "Period" xml) Integer.parse);
          unit =
            (Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.unit
              (fun f -> Query.Pair ("Unit", (StandardUnit.to_query f)));
           Util.option_map v.period
             (fun f -> Query.Pair ("Period", (Integer.to_query f)));
           Some
             (Query.Pair
                ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Util.option_map v.extended_statistic
             (fun f -> Query.Pair ("ExtendedStatistic", (String.to_query f)));
           Util.option_map v.statistic
             (fun f -> Query.Pair ("Statistic", (Statistic.to_query f)));
           Some (Query.Pair ("Namespace", (String.to_query v.namespace)));
           Some (Query.Pair ("MetricName", (String.to_query v.metric_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.unit
              (fun f -> ("unit", (StandardUnit.to_json f)));
           Util.option_map v.period
             (fun f -> ("period", (Integer.to_json f)));
           Some ("dimensions", (Dimensions.to_json v.dimensions));
           Util.option_map v.extended_statistic
             (fun f -> ("extended_statistic", (String.to_json f)));
           Util.option_map v.statistic
             (fun f -> ("statistic", (Statistic.to_json f)));
           Some ("namespace", (String.to_json v.namespace));
           Some ("metric_name", (String.to_json v.metric_name))])
    let of_json j =
      {
        metric_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "metric_name")));
        namespace =
          (String.of_json (Util.of_option_exn (Json.lookup j "namespace")));
        statistic =
          (Util.option_map (Json.lookup j "statistic") Statistic.of_json);
        extended_statistic =
          (Util.option_map (Json.lookup j "extended_statistic")
             String.of_json);
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")));
        period = (Util.option_map (Json.lookup j "period") Integer.of_json);
        unit = (Util.option_map (Json.lookup j "unit") StandardUnit.of_json)
      }
  end
module GetInsightRuleReportOutput =
  struct
    type t =
      {
      key_labels: InsightRuleContributorKeyLabels.t ;
      aggregation_statistic: String.t option ;
      aggregate_value: Double.t option ;
      approximate_unique_count: Long.t option ;
      contributors: InsightRuleContributors.t ;
      metric_datapoints: InsightRuleMetricDatapoints.t }
    let make ?(key_labels= [])  ?aggregation_statistic  ?aggregate_value 
      ?approximate_unique_count  ?(contributors= [])  ?(metric_datapoints=
      [])  () =
      {
        key_labels;
        aggregation_statistic;
        aggregate_value;
        approximate_unique_count;
        contributors;
        metric_datapoints
      }
    let parse xml =
      Some
        {
          key_labels =
            (Util.of_option []
               (Util.option_bind (Xml.member "KeyLabels" xml)
                  InsightRuleContributorKeyLabels.parse));
          aggregation_statistic =
            (Util.option_bind (Xml.member "AggregationStatistic" xml)
               String.parse);
          aggregate_value =
            (Util.option_bind (Xml.member "AggregateValue" xml) Double.parse);
          approximate_unique_count =
            (Util.option_bind (Xml.member "ApproximateUniqueCount" xml)
               Long.parse);
          contributors =
            (Util.of_option []
               (Util.option_bind (Xml.member "Contributors" xml)
                  InsightRuleContributors.parse));
          metric_datapoints =
            (Util.of_option []
               (Util.option_bind (Xml.member "MetricDatapoints" xml)
                  InsightRuleMetricDatapoints.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("MetricDatapoints.member",
                   (InsightRuleMetricDatapoints.to_query v.metric_datapoints)));
           Some
             (Query.Pair
                ("Contributors.member",
                  (InsightRuleContributors.to_query v.contributors)));
           Util.option_map v.approximate_unique_count
             (fun f ->
                Query.Pair ("ApproximateUniqueCount", (Long.to_query f)));
           Util.option_map v.aggregate_value
             (fun f -> Query.Pair ("AggregateValue", (Double.to_query f)));
           Util.option_map v.aggregation_statistic
             (fun f ->
                Query.Pair ("AggregationStatistic", (String.to_query f)));
           Some
             (Query.Pair
                ("KeyLabels.member",
                  (InsightRuleContributorKeyLabels.to_query v.key_labels)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("metric_datapoints",
                (InsightRuleMetricDatapoints.to_json v.metric_datapoints));
           Some
             ("contributors",
               (InsightRuleContributors.to_json v.contributors));
           Util.option_map v.approximate_unique_count
             (fun f -> ("approximate_unique_count", (Long.to_json f)));
           Util.option_map v.aggregate_value
             (fun f -> ("aggregate_value", (Double.to_json f)));
           Util.option_map v.aggregation_statistic
             (fun f -> ("aggregation_statistic", (String.to_json f)));
           Some
             ("key_labels",
               (InsightRuleContributorKeyLabels.to_json v.key_labels))])
    let of_json j =
      {
        key_labels =
          (InsightRuleContributorKeyLabels.of_json
             (Util.of_option_exn (Json.lookup j "key_labels")));
        aggregation_statistic =
          (Util.option_map (Json.lookup j "aggregation_statistic")
             String.of_json);
        aggregate_value =
          (Util.option_map (Json.lookup j "aggregate_value") Double.of_json);
        approximate_unique_count =
          (Util.option_map (Json.lookup j "approximate_unique_count")
             Long.of_json);
        contributors =
          (InsightRuleContributors.of_json
             (Util.of_option_exn (Json.lookup j "contributors")));
        metric_datapoints =
          (InsightRuleMetricDatapoints.of_json
             (Util.of_option_exn (Json.lookup j "metric_datapoints")))
      }
  end
module DashboardInvalidInputError =
  struct
    type t =
      {
      message: String.t option ;
      dashboard_validation_messages: DashboardValidationMessages.t }
    let make ?message  ?(dashboard_validation_messages= [])  () =
      { message; dashboard_validation_messages }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse);
          dashboard_validation_messages =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "dashboardValidationMessages" xml)
                  DashboardValidationMessages.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("dashboardValidationMessages.member",
                   (DashboardValidationMessages.to_query
                      v.dashboard_validation_messages)));
           Util.option_map v.message
             (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("dashboard_validation_messages",
                (DashboardValidationMessages.to_json
                   v.dashboard_validation_messages));
           Util.option_map v.message
             (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      {
        message = (Util.option_map (Json.lookup j "message") String.of_json);
        dashboard_validation_messages =
          (DashboardValidationMessages.of_json
             (Util.of_option_exn
                (Json.lookup j "dashboard_validation_messages")))
      }
  end
module DeleteInsightRulesInput =
  struct
    type t = {
      rule_names: InsightRuleNames.t }
    let make ~rule_names  () = { rule_names }
    let parse xml =
      Some
        {
          rule_names =
            (Xml.required "RuleNames"
               (Util.option_bind (Xml.member "RuleNames" xml)
                  InsightRuleNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RuleNames.member",
                   (InsightRuleNames.to_query v.rule_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("rule_names", (InsightRuleNames.to_json v.rule_names))])
    let of_json j =
      {
        rule_names =
          (InsightRuleNames.of_json
             (Util.of_option_exn (Json.lookup j "rule_names")))
      }
  end
module InvalidParameterValueException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DescribeAnomalyDetectorsOutput =
  struct
    type t =
      {
      anomaly_detectors: AnomalyDetectors.t ;
      next_token: String.t option }
    let make ?(anomaly_detectors= [])  ?next_token  () =
      { anomaly_detectors; next_token }
    let parse xml =
      Some
        {
          anomaly_detectors =
            (Util.of_option []
               (Util.option_bind (Xml.member "AnomalyDetectors" xml)
                  AnomalyDetectors.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("AnomalyDetectors.member",
                  (AnomalyDetectors.to_query v.anomaly_detectors)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("anomaly_detectors",
               (AnomalyDetectors.to_json v.anomaly_detectors))])
    let of_json j =
      {
        anomaly_detectors =
          (AnomalyDetectors.of_json
             (Util.of_option_exn (Json.lookup j "anomaly_detectors")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module PutAnomalyDetectorOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module PutDashboardInput =
  struct
    type t = {
      dashboard_name: String.t ;
      dashboard_body: String.t }
    let make ~dashboard_name  ~dashboard_body  () =
      { dashboard_name; dashboard_body }
    let parse xml =
      Some
        {
          dashboard_name =
            (Xml.required "DashboardName"
               (Util.option_bind (Xml.member "DashboardName" xml)
                  String.parse));
          dashboard_body =
            (Xml.required "DashboardBody"
               (Util.option_bind (Xml.member "DashboardBody" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DashboardBody", (String.to_query v.dashboard_body)));
           Some
             (Query.Pair
                ("DashboardName", (String.to_query v.dashboard_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("dashboard_body", (String.to_json v.dashboard_body));
           Some ("dashboard_name", (String.to_json v.dashboard_name))])
    let of_json j =
      {
        dashboard_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "dashboard_name")));
        dashboard_body =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "dashboard_body")))
      }
  end
module ResourceNotFound =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DeleteDashboardsOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeAlarmsOutput =
  struct
    type t =
      {
      composite_alarms: CompositeAlarms.t ;
      metric_alarms: MetricAlarms.t ;
      next_token: String.t option }
    let make ?(composite_alarms= [])  ?(metric_alarms= [])  ?next_token  () =
      { composite_alarms; metric_alarms; next_token }
    let parse xml =
      Some
        {
          composite_alarms =
            (Util.of_option []
               (Util.option_bind (Xml.member "CompositeAlarms" xml)
                  CompositeAlarms.parse));
          metric_alarms =
            (Util.of_option []
               (Util.option_bind (Xml.member "MetricAlarms" xml)
                  MetricAlarms.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("MetricAlarms.member",
                  (MetricAlarms.to_query v.metric_alarms)));
           Some
             (Query.Pair
                ("CompositeAlarms.member",
                  (CompositeAlarms.to_query v.composite_alarms)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("metric_alarms", (MetricAlarms.to_json v.metric_alarms));
           Some
             ("composite_alarms",
               (CompositeAlarms.to_json v.composite_alarms))])
    let of_json j =
      {
        composite_alarms =
          (CompositeAlarms.of_json
             (Util.of_option_exn (Json.lookup j "composite_alarms")));
        metric_alarms =
          (MetricAlarms.of_json
             (Util.of_option_exn (Json.lookup j "metric_alarms")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DescribeInsightRulesInput =
  struct
    type t = {
      next_token: String.t option ;
      max_results: Integer.t option }
    let make ?next_token  ?max_results  () = { next_token; max_results }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json)
      }
  end
module PutInsightRuleInput =
  struct
    type t =
      {
      rule_name: String.t ;
      rule_state: String.t option ;
      rule_definition: String.t ;
      tags: TagList.t }
    let make ~rule_name  ?rule_state  ~rule_definition  ?(tags= [])  () =
      { rule_name; rule_state; rule_definition; tags }
    let parse xml =
      Some
        {
          rule_name =
            (Xml.required "RuleName"
               (Util.option_bind (Xml.member "RuleName" xml) String.parse));
          rule_state =
            (Util.option_bind (Xml.member "RuleState" xml) String.parse);
          rule_definition =
            (Xml.required "RuleDefinition"
               (Util.option_bind (Xml.member "RuleDefinition" xml)
                  String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("RuleDefinition", (String.to_query v.rule_definition)));
           Util.option_map v.rule_state
             (fun f -> Query.Pair ("RuleState", (String.to_query f)));
           Some (Query.Pair ("RuleName", (String.to_query v.rule_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some ("rule_definition", (String.to_json v.rule_definition));
           Util.option_map v.rule_state
             (fun f -> ("rule_state", (String.to_json f)));
           Some ("rule_name", (String.to_json v.rule_name))])
    let of_json j =
      {
        rule_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "rule_name")));
        rule_state =
          (Util.option_map (Json.lookup j "rule_state") String.of_json);
        rule_definition =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "rule_definition")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module GetMetricDataOutput =
  struct
    type t =
      {
      metric_data_results: MetricDataResults.t ;
      next_token: String.t option ;
      messages: MetricDataResultMessages.t }
    let make ?(metric_data_results= [])  ?next_token  ?(messages= [])  () =
      { metric_data_results; next_token; messages }
    let parse xml =
      Some
        {
          metric_data_results =
            (Util.of_option []
               (Util.option_bind (Xml.member "MetricDataResults" xml)
                  MetricDataResults.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          messages =
            (Util.of_option []
               (Util.option_bind (Xml.member "Messages" xml)
                  MetricDataResultMessages.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Messages.member",
                   (MetricDataResultMessages.to_query v.messages)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("MetricDataResults.member",
                  (MetricDataResults.to_query v.metric_data_results)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("messages", (MetricDataResultMessages.to_json v.messages));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some
             ("metric_data_results",
               (MetricDataResults.to_json v.metric_data_results))])
    let of_json j =
      {
        metric_data_results =
          (MetricDataResults.of_json
             (Util.of_option_exn (Json.lookup j "metric_data_results")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        messages =
          (MetricDataResultMessages.of_json
             (Util.of_option_exn (Json.lookup j "messages")))
      }
  end
module InvalidNextToken =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListDashboardsOutput =
  struct
    type t =
      {
      dashboard_entries: DashboardEntries.t ;
      next_token: String.t option }
    let make ?(dashboard_entries= [])  ?next_token  () =
      { dashboard_entries; next_token }
    let parse xml =
      Some
        {
          dashboard_entries =
            (Util.of_option []
               (Util.option_bind (Xml.member "DashboardEntries" xml)
                  DashboardEntries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("DashboardEntries.member",
                  (DashboardEntries.to_query v.dashboard_entries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("dashboard_entries",
               (DashboardEntries.to_json v.dashboard_entries))])
    let of_json j =
      {
        dashboard_entries =
          (DashboardEntries.of_json
             (Util.of_option_exn (Json.lookup j "dashboard_entries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module InvalidParameterCombinationException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module MissingRequiredParameterException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module PutDashboardOutput =
  struct
    type t = {
      dashboard_validation_messages: DashboardValidationMessages.t }
    let make ?(dashboard_validation_messages= [])  () =
      { dashboard_validation_messages }
    let parse xml =
      Some
        {
          dashboard_validation_messages =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "DashboardValidationMessages" xml)
                  DashboardValidationMessages.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DashboardValidationMessages.member",
                   (DashboardValidationMessages.to_query
                      v.dashboard_validation_messages)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("dashboard_validation_messages",
                (DashboardValidationMessages.to_json
                   v.dashboard_validation_messages))])
    let of_json j =
      {
        dashboard_validation_messages =
          (DashboardValidationMessages.of_json
             (Util.of_option_exn
                (Json.lookup j "dashboard_validation_messages")))
      }
  end
module ListMetricsOutput =
  struct
    type t = {
      metrics: Metrics.t ;
      next_token: String.t option }
    let make ?(metrics= [])  ?next_token  () = { metrics; next_token }
    let parse xml =
      Some
        {
          metrics =
            (Util.of_option []
               (Util.option_bind (Xml.member "Metrics" xml) Metrics.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("Metrics.member", (Metrics.to_query v.metrics)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("metrics", (Metrics.to_json v.metrics))])
    let of_json j =
      {
        metrics =
          (Metrics.of_json (Util.of_option_exn (Json.lookup j "metrics")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module TagResourceOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteAnomalyDetectorInput =
  struct
    type t =
      {
      namespace: String.t ;
      metric_name: String.t ;
      dimensions: Dimensions.t ;
      stat: String.t }
    let make ~namespace  ~metric_name  ?(dimensions= [])  ~stat  () =
      { namespace; metric_name; dimensions; stat }
    let parse xml =
      Some
        {
          namespace =
            (Xml.required "Namespace"
               (Util.option_bind (Xml.member "Namespace" xml) String.parse));
          metric_name =
            (Xml.required "MetricName"
               (Util.option_bind (Xml.member "MetricName" xml) String.parse));
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse));
          stat =
            (Xml.required "Stat"
               (Util.option_bind (Xml.member "Stat" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Stat", (String.to_query v.stat)));
           Some
             (Query.Pair
                ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Some (Query.Pair ("MetricName", (String.to_query v.metric_name)));
           Some (Query.Pair ("Namespace", (String.to_query v.namespace)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("stat", (String.to_json v.stat));
           Some ("dimensions", (Dimensions.to_json v.dimensions));
           Some ("metric_name", (String.to_json v.metric_name));
           Some ("namespace", (String.to_json v.namespace))])
    let of_json j =
      {
        namespace =
          (String.of_json (Util.of_option_exn (Json.lookup j "namespace")));
        metric_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "metric_name")));
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")));
        stat = (String.of_json (Util.of_option_exn (Json.lookup j "stat")))
      }
  end
module DeleteInsightRulesOutput =
  struct
    type t = {
      failures: BatchFailures.t }
    let make ?(failures= [])  () = { failures }
    let parse xml =
      Some
        {
          failures =
            (Util.of_option []
               (Util.option_bind (Xml.member "Failures" xml)
                  BatchFailures.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Failures.member", (BatchFailures.to_query v.failures)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("failures", (BatchFailures.to_json v.failures))])
    let of_json j =
      {
        failures =
          (BatchFailures.of_json
             (Util.of_option_exn (Json.lookup j "failures")))
      }
  end
module LimitExceededException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module PutInsightRuleOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ResourceNotFoundException =
  struct
    type t = {
      resource_type: String.t option ;
      resource_id: String.t option }
    let make ?resource_type  ?resource_id  () =
      { resource_type; resource_id }
    let parse xml =
      Some
        {
          resource_type =
            (Util.option_bind (Xml.member "ResourceType" xml) String.parse);
          resource_id =
            (Util.option_bind (Xml.member "ResourceId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.resource_id
              (fun f -> Query.Pair ("ResourceId", (String.to_query f)));
           Util.option_map v.resource_type
             (fun f -> Query.Pair ("ResourceType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.resource_id
              (fun f -> ("resource_id", (String.to_json f)));
           Util.option_map v.resource_type
             (fun f -> ("resource_type", (String.to_json f)))])
    let of_json j =
      {
        resource_type =
          (Util.option_map (Json.lookup j "resource_type") String.of_json);
        resource_id =
          (Util.option_map (Json.lookup j "resource_id") String.of_json)
      }
  end
module DeleteAnomalyDetectorOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidFormatFault =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DashboardNotFoundError =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DescribeInsightRulesOutput =
  struct
    type t = {
      next_token: String.t option ;
      insight_rules: InsightRules.t }
    let make ?next_token  ?(insight_rules= [])  () =
      { next_token; insight_rules }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          insight_rules =
            (Util.of_option []
               (Util.option_bind (Xml.member "InsightRules" xml)
                  InsightRules.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InsightRules.member",
                   (InsightRules.to_query v.insight_rules)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("insight_rules", (InsightRules.to_json v.insight_rules));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        insight_rules =
          (InsightRules.of_json
             (Util.of_option_exn (Json.lookup j "insight_rules")))
      }
  end
module PutCompositeAlarmInput =
  struct
    type t =
      {
      actions_enabled: Boolean.t option ;
      alarm_actions: ResourceList.t ;
      alarm_description: String.t option ;
      alarm_name: String.t ;
      alarm_rule: String.t ;
      insufficient_data_actions: ResourceList.t ;
      o_k_actions: ResourceList.t ;
      tags: TagList.t }
    let make ?actions_enabled  ?(alarm_actions= [])  ?alarm_description 
      ~alarm_name  ~alarm_rule  ?(insufficient_data_actions= []) 
      ?(o_k_actions= [])  ?(tags= [])  () =
      {
        actions_enabled;
        alarm_actions;
        alarm_description;
        alarm_name;
        alarm_rule;
        insufficient_data_actions;
        o_k_actions;
        tags
      }
    let parse xml =
      Some
        {
          actions_enabled =
            (Util.option_bind (Xml.member "ActionsEnabled" xml) Boolean.parse);
          alarm_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "AlarmActions" xml)
                  ResourceList.parse));
          alarm_description =
            (Util.option_bind (Xml.member "AlarmDescription" xml)
               String.parse);
          alarm_name =
            (Xml.required "AlarmName"
               (Util.option_bind (Xml.member "AlarmName" xml) String.parse));
          alarm_rule =
            (Xml.required "AlarmRule"
               (Util.option_bind (Xml.member "AlarmRule" xml) String.parse));
          insufficient_data_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "InsufficientDataActions" xml)
                  ResourceList.parse));
          o_k_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "OKActions" xml)
                  ResourceList.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("OKActions.member", (ResourceList.to_query v.o_k_actions)));
           Some
             (Query.Pair
                ("InsufficientDataActions.member",
                  (ResourceList.to_query v.insufficient_data_actions)));
           Some (Query.Pair ("AlarmRule", (String.to_query v.alarm_rule)));
           Some (Query.Pair ("AlarmName", (String.to_query v.alarm_name)));
           Util.option_map v.alarm_description
             (fun f -> Query.Pair ("AlarmDescription", (String.to_query f)));
           Some
             (Query.Pair
                ("AlarmActions.member",
                  (ResourceList.to_query v.alarm_actions)));
           Util.option_map v.actions_enabled
             (fun f -> Query.Pair ("ActionsEnabled", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some ("o_k_actions", (ResourceList.to_json v.o_k_actions));
           Some
             ("insufficient_data_actions",
               (ResourceList.to_json v.insufficient_data_actions));
           Some ("alarm_rule", (String.to_json v.alarm_rule));
           Some ("alarm_name", (String.to_json v.alarm_name));
           Util.option_map v.alarm_description
             (fun f -> ("alarm_description", (String.to_json f)));
           Some ("alarm_actions", (ResourceList.to_json v.alarm_actions));
           Util.option_map v.actions_enabled
             (fun f -> ("actions_enabled", (Boolean.to_json f)))])
    let of_json j =
      {
        actions_enabled =
          (Util.option_map (Json.lookup j "actions_enabled") Boolean.of_json);
        alarm_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "alarm_actions")));
        alarm_description =
          (Util.option_map (Json.lookup j "alarm_description") String.of_json);
        alarm_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "alarm_name")));
        alarm_rule =
          (String.of_json (Util.of_option_exn (Json.lookup j "alarm_rule")));
        insufficient_data_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "insufficient_data_actions")));
        o_k_actions =
          (ResourceList.of_json
             (Util.of_option_exn (Json.lookup j "o_k_actions")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module GetMetricStatisticsInput =
  struct
    type t =
      {
      namespace: String.t ;
      metric_name: String.t ;
      dimensions: Dimensions.t ;
      start_time: DateTime.t ;
      end_time: DateTime.t ;
      period: Integer.t ;
      statistics: Statistics.t ;
      extended_statistics: ExtendedStatistics.t ;
      unit: StandardUnit.t option }
    let make ~namespace  ~metric_name  ?(dimensions= [])  ~start_time 
      ~end_time  ~period  ?(statistics= [])  ?(extended_statistics= []) 
      ?unit  () =
      {
        namespace;
        metric_name;
        dimensions;
        start_time;
        end_time;
        period;
        statistics;
        extended_statistics;
        unit
      }
    let parse xml =
      Some
        {
          namespace =
            (Xml.required "Namespace"
               (Util.option_bind (Xml.member "Namespace" xml) String.parse));
          metric_name =
            (Xml.required "MetricName"
               (Util.option_bind (Xml.member "MetricName" xml) String.parse));
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse));
          start_time =
            (Xml.required "StartTime"
               (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse));
          end_time =
            (Xml.required "EndTime"
               (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse));
          period =
            (Xml.required "Period"
               (Util.option_bind (Xml.member "Period" xml) Integer.parse));
          statistics =
            (Util.of_option []
               (Util.option_bind (Xml.member "Statistics" xml)
                  Statistics.parse));
          extended_statistics =
            (Util.of_option []
               (Util.option_bind (Xml.member "ExtendedStatistics" xml)
                  ExtendedStatistics.parse));
          unit =
            (Util.option_bind (Xml.member "Unit" xml) StandardUnit.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.unit
              (fun f -> Query.Pair ("Unit", (StandardUnit.to_query f)));
           Some
             (Query.Pair
                ("ExtendedStatistics.member",
                  (ExtendedStatistics.to_query v.extended_statistics)));
           Some
             (Query.Pair
                ("Statistics.member", (Statistics.to_query v.statistics)));
           Some (Query.Pair ("Period", (Integer.to_query v.period)));
           Some (Query.Pair ("EndTime", (DateTime.to_query v.end_time)));
           Some (Query.Pair ("StartTime", (DateTime.to_query v.start_time)));
           Some
             (Query.Pair
                ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Some (Query.Pair ("MetricName", (String.to_query v.metric_name)));
           Some (Query.Pair ("Namespace", (String.to_query v.namespace)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.unit
              (fun f -> ("unit", (StandardUnit.to_json f)));
           Some
             ("extended_statistics",
               (ExtendedStatistics.to_json v.extended_statistics));
           Some ("statistics", (Statistics.to_json v.statistics));
           Some ("period", (Integer.to_json v.period));
           Some ("end_time", (DateTime.to_json v.end_time));
           Some ("start_time", (DateTime.to_json v.start_time));
           Some ("dimensions", (Dimensions.to_json v.dimensions));
           Some ("metric_name", (String.to_json v.metric_name));
           Some ("namespace", (String.to_json v.namespace))])
    let of_json j =
      {
        namespace =
          (String.of_json (Util.of_option_exn (Json.lookup j "namespace")));
        metric_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "metric_name")));
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")));
        start_time =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "start_time")));
        end_time =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "end_time")));
        period =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "period")));
        statistics =
          (Statistics.of_json
             (Util.of_option_exn (Json.lookup j "statistics")));
        extended_statistics =
          (ExtendedStatistics.of_json
             (Util.of_option_exn (Json.lookup j "extended_statistics")));
        unit = (Util.option_map (Json.lookup j "unit") StandardUnit.of_json)
      }
  end
module PutMetricDataInput =
  struct
    type t = {
      namespace: String.t ;
      metric_data: MetricData.t }
    let make ~namespace  ~metric_data  () = { namespace; metric_data }
    let parse xml =
      Some
        {
          namespace =
            (Xml.required "Namespace"
               (Util.option_bind (Xml.member "Namespace" xml) String.parse));
          metric_data =
            (Xml.required "MetricData"
               (Util.option_bind (Xml.member "MetricData" xml)
                  MetricData.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("MetricData.member", (MetricData.to_query v.metric_data)));
           Some (Query.Pair ("Namespace", (String.to_query v.namespace)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("metric_data", (MetricData.to_json v.metric_data));
           Some ("namespace", (String.to_json v.namespace))])
    let of_json j =
      {
        namespace =
          (String.of_json (Util.of_option_exn (Json.lookup j "namespace")));
        metric_data =
          (MetricData.of_json
             (Util.of_option_exn (Json.lookup j "metric_data")))
      }
  end
module DescribeAlarmsForMetricOutput =
  struct
    type t = {
      metric_alarms: MetricAlarms.t }
    let make ?(metric_alarms= [])  () = { metric_alarms }
    let parse xml =
      Some
        {
          metric_alarms =
            (Util.of_option []
               (Util.option_bind (Xml.member "MetricAlarms" xml)
                  MetricAlarms.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("MetricAlarms.member",
                   (MetricAlarms.to_query v.metric_alarms)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("metric_alarms", (MetricAlarms.to_json v.metric_alarms))])
    let of_json j =
      {
        metric_alarms =
          (MetricAlarms.of_json
             (Util.of_option_exn (Json.lookup j "metric_alarms")))
      }
  end
module DeleteAlarmsInput =
  struct
    type t = {
      alarm_names: AlarmNames.t }
    let make ~alarm_names  () = { alarm_names }
    let parse xml =
      Some
        {
          alarm_names =
            (Xml.required "AlarmNames"
               (Util.option_bind (Xml.member "AlarmNames" xml)
                  AlarmNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AlarmNames.member", (AlarmNames.to_query v.alarm_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("alarm_names", (AlarmNames.to_json v.alarm_names))])
    let of_json j =
      {
        alarm_names =
          (AlarmNames.of_json
             (Util.of_option_exn (Json.lookup j "alarm_names")))
      }
  end
module DescribeAnomalyDetectorsInput =
  struct
    type t =
      {
      next_token: String.t option ;
      max_results: Integer.t option ;
      namespace: String.t option ;
      metric_name: String.t option ;
      dimensions: Dimensions.t }
    let make ?next_token  ?max_results  ?namespace  ?metric_name 
      ?(dimensions= [])  () =
      { next_token; max_results; namespace; metric_name; dimensions }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          namespace =
            (Util.option_bind (Xml.member "Namespace" xml) String.parse);
          metric_name =
            (Util.option_bind (Xml.member "MetricName" xml) String.parse);
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Util.option_map v.metric_name
             (fun f -> Query.Pair ("MetricName", (String.to_query f)));
           Util.option_map v.namespace
             (fun f -> Query.Pair ("Namespace", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("dimensions", (Dimensions.to_json v.dimensions));
           Util.option_map v.metric_name
             (fun f -> ("metric_name", (String.to_json f)));
           Util.option_map v.namespace
             (fun f -> ("namespace", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        namespace =
          (Util.option_map (Json.lookup j "namespace") String.of_json);
        metric_name =
          (Util.option_map (Json.lookup j "metric_name") String.of_json);
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")))
      }
  end
module ListTagsForResourceOutput =
  struct
    type t = {
      tags: TagList.t }
    let make ?(tags= [])  () = { tags }
    let parse xml =
      Some
        {
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("tags", (TagList.to_json v.tags))])
    let of_json j =
      { tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module PutAnomalyDetectorInput =
  struct
    type t =
      {
      namespace: String.t ;
      metric_name: String.t ;
      dimensions: Dimensions.t ;
      stat: String.t ;
      configuration: AnomalyDetectorConfiguration.t option }
    let make ~namespace  ~metric_name  ?(dimensions= [])  ~stat 
      ?configuration  () =
      { namespace; metric_name; dimensions; stat; configuration }
    let parse xml =
      Some
        {
          namespace =
            (Xml.required "Namespace"
               (Util.option_bind (Xml.member "Namespace" xml) String.parse));
          metric_name =
            (Xml.required "MetricName"
               (Util.option_bind (Xml.member "MetricName" xml) String.parse));
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  Dimensions.parse));
          stat =
            (Xml.required "Stat"
               (Util.option_bind (Xml.member "Stat" xml) String.parse));
          configuration =
            (Util.option_bind (Xml.member "Configuration" xml)
               AnomalyDetectorConfiguration.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.configuration
              (fun f ->
                 Query.Pair
                   ("Configuration",
                     (AnomalyDetectorConfiguration.to_query f)));
           Some (Query.Pair ("Stat", (String.to_query v.stat)));
           Some
             (Query.Pair
                ("Dimensions.member", (Dimensions.to_query v.dimensions)));
           Some (Query.Pair ("MetricName", (String.to_query v.metric_name)));
           Some (Query.Pair ("Namespace", (String.to_query v.namespace)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.configuration
              (fun f ->
                 ("configuration", (AnomalyDetectorConfiguration.to_json f)));
           Some ("stat", (String.to_json v.stat));
           Some ("dimensions", (Dimensions.to_json v.dimensions));
           Some ("metric_name", (String.to_json v.metric_name));
           Some ("namespace", (String.to_json v.namespace))])
    let of_json j =
      {
        namespace =
          (String.of_json (Util.of_option_exn (Json.lookup j "namespace")));
        metric_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "metric_name")));
        dimensions =
          (Dimensions.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")));
        stat = (String.of_json (Util.of_option_exn (Json.lookup j "stat")));
        configuration =
          (Util.option_map (Json.lookup j "configuration")
             AnomalyDetectorConfiguration.of_json)
      }
  end
module DeleteDashboardsInput =
  struct
    type t = {
      dashboard_names: DashboardNames.t }
    let make ~dashboard_names  () = { dashboard_names }
    let parse xml =
      Some
        {
          dashboard_names =
            (Xml.required "DashboardNames"
               (Util.option_bind (Xml.member "DashboardNames" xml)
                  DashboardNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DashboardNames.member",
                   (DashboardNames.to_query v.dashboard_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("dashboard_names", (DashboardNames.to_json v.dashboard_names))])
    let of_json j =
      {
        dashboard_names =
          (DashboardNames.of_json
             (Util.of_option_exn (Json.lookup j "dashboard_names")))
      }
  end
module DisableAlarmActionsInput =
  struct
    type t = {
      alarm_names: AlarmNames.t }
    let make ~alarm_names  () = { alarm_names }
    let parse xml =
      Some
        {
          alarm_names =
            (Xml.required "AlarmNames"
               (Util.option_bind (Xml.member "AlarmNames" xml)
                  AlarmNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AlarmNames.member", (AlarmNames.to_query v.alarm_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("alarm_names", (AlarmNames.to_json v.alarm_names))])
    let of_json j =
      {
        alarm_names =
          (AlarmNames.of_json
             (Util.of_option_exn (Json.lookup j "alarm_names")))
      }
  end
module GetMetricStatisticsOutput =
  struct
    type t = {
      label: String.t option ;
      datapoints: Datapoints.t }
    let make ?label  ?(datapoints= [])  () = { label; datapoints }
    let parse xml =
      Some
        {
          label = (Util.option_bind (Xml.member "Label" xml) String.parse);
          datapoints =
            (Util.of_option []
               (Util.option_bind (Xml.member "Datapoints" xml)
                  Datapoints.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Datapoints.member", (Datapoints.to_query v.datapoints)));
           Util.option_map v.label
             (fun f -> Query.Pair ("Label", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("datapoints", (Datapoints.to_json v.datapoints));
           Util.option_map v.label (fun f -> ("label", (String.to_json f)))])
    let of_json j =
      {
        label = (Util.option_map (Json.lookup j "label") String.of_json);
        datapoints =
          (Datapoints.of_json
             (Util.of_option_exn (Json.lookup j "datapoints")))
      }
  end