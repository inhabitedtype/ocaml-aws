open Aws
open Aws.BaseTypes
open CalendarLib

type calendar = Calendar.t

module MetricDimension = struct
  type t =
    { key : String.t option
    ; value : String.t option
    }

  let make ?key ?value () = { key; value }

  let parse xml =
    Some
      { key = Util.option_bind (Xml.member "Key" xml) String.parse
      ; value = Util.option_bind (Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> Query.Pair ("Value", String.to_query f))
         ; Util.option_map v.key (fun f -> Query.Pair ("Key", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> "value", String.to_json f)
         ; Util.option_map v.key (fun f -> "key", String.to_json f)
         ])

  let of_json j =
    { key = Util.option_map (Json.lookup j "key") String.of_json
    ; value = Util.option_map (Json.lookup j "value") String.of_json
    }
end

module VolumeSpecification = struct
  type t =
    { volume_type : String.t
    ; iops : Integer.t option
    ; size_in_g_b : Integer.t
    }

  let make ~volume_type ?iops ~size_in_g_b () = { volume_type; iops; size_in_g_b }

  let parse xml =
    Some
      { volume_type =
          Xml.required
            "VolumeType"
            (Util.option_bind (Xml.member "VolumeType" xml) String.parse)
      ; iops = Util.option_bind (Xml.member "Iops" xml) Integer.parse
      ; size_in_g_b =
          Xml.required
            "SizeInGB"
            (Util.option_bind (Xml.member "SizeInGB" xml) Integer.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("SizeInGB", Integer.to_query v.size_in_g_b))
         ; Util.option_map v.iops (fun f -> Query.Pair ("Iops", Integer.to_query f))
         ; Some (Query.Pair ("VolumeType", String.to_query v.volume_type))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("size_in_g_b", Integer.to_json v.size_in_g_b)
         ; Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
         ; Some ("volume_type", String.to_json v.volume_type)
         ])

  let of_json j =
    { volume_type = String.of_json (Util.of_option_exn (Json.lookup j "volume_type"))
    ; iops = Util.option_map (Json.lookup j "iops") Integer.of_json
    ; size_in_g_b = Integer.of_json (Util.of_option_exn (Json.lookup j "size_in_g_b"))
    }
end

module AdjustmentType = struct
  type t =
    | CHANGE_IN_CAPACITY
    | PERCENT_CHANGE_IN_CAPACITY
    | EXACT_CAPACITY

  let str_to_t =
    [ "EXACT_CAPACITY", EXACT_CAPACITY
    ; "PERCENT_CHANGE_IN_CAPACITY", PERCENT_CHANGE_IN_CAPACITY
    ; "CHANGE_IN_CAPACITY", CHANGE_IN_CAPACITY
    ]

  let t_to_str =
    [ EXACT_CAPACITY, "EXACT_CAPACITY"
    ; PERCENT_CHANGE_IN_CAPACITY, "PERCENT_CHANGE_IN_CAPACITY"
    ; CHANGE_IN_CAPACITY, "CHANGE_IN_CAPACITY"
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
    | GREATER_THAN_OR_EQUAL
    | GREATER_THAN
    | LESS_THAN
    | LESS_THAN_OR_EQUAL

  let str_to_t =
    [ "LESS_THAN_OR_EQUAL", LESS_THAN_OR_EQUAL
    ; "LESS_THAN", LESS_THAN
    ; "GREATER_THAN", GREATER_THAN
    ; "GREATER_THAN_OR_EQUAL", GREATER_THAN_OR_EQUAL
    ]

  let t_to_str =
    [ LESS_THAN_OR_EQUAL, "LESS_THAN_OR_EQUAL"
    ; LESS_THAN, "LESS_THAN"
    ; GREATER_THAN, "GREATER_THAN"
    ; GREATER_THAN_OR_EQUAL, "GREATER_THAN_OR_EQUAL"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module MetricDimensionList = struct
  type t = MetricDimension.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map MetricDimension.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list MetricDimension.to_query v

  let to_json v = `List (List.map MetricDimension.to_json v)

  let of_json j = Json.to_list MetricDimension.of_json j
end

module Statistic = struct
  type t =
    | SAMPLE_COUNT
    | AVERAGE
    | SUM
    | MINIMUM
    | MAXIMUM

  let str_to_t =
    [ "MAXIMUM", MAXIMUM
    ; "MINIMUM", MINIMUM
    ; "SUM", SUM
    ; "AVERAGE", AVERAGE
    ; "SAMPLE_COUNT", SAMPLE_COUNT
    ]

  let t_to_str =
    [ MAXIMUM, "MAXIMUM"
    ; MINIMUM, "MINIMUM"
    ; SUM, "SUM"
    ; AVERAGE, "AVERAGE"
    ; SAMPLE_COUNT, "SAMPLE_COUNT"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module Unit = struct
  type t =
    | NONE
    | SECONDS
    | MICRO_SECONDS
    | MILLI_SECONDS
    | BYTES
    | KILO_BYTES
    | MEGA_BYTES
    | GIGA_BYTES
    | TERA_BYTES
    | BITS
    | KILO_BITS
    | MEGA_BITS
    | GIGA_BITS
    | TERA_BITS
    | PERCENT
    | COUNT
    | BYTES_PER_SECOND
    | KILO_BYTES_PER_SECOND
    | MEGA_BYTES_PER_SECOND
    | GIGA_BYTES_PER_SECOND
    | TERA_BYTES_PER_SECOND
    | BITS_PER_SECOND
    | KILO_BITS_PER_SECOND
    | MEGA_BITS_PER_SECOND
    | GIGA_BITS_PER_SECOND
    | TERA_BITS_PER_SECOND
    | COUNT_PER_SECOND

  let str_to_t =
    [ "COUNT_PER_SECOND", COUNT_PER_SECOND
    ; "TERA_BITS_PER_SECOND", TERA_BITS_PER_SECOND
    ; "GIGA_BITS_PER_SECOND", GIGA_BITS_PER_SECOND
    ; "MEGA_BITS_PER_SECOND", MEGA_BITS_PER_SECOND
    ; "KILO_BITS_PER_SECOND", KILO_BITS_PER_SECOND
    ; "BITS_PER_SECOND", BITS_PER_SECOND
    ; "TERA_BYTES_PER_SECOND", TERA_BYTES_PER_SECOND
    ; "GIGA_BYTES_PER_SECOND", GIGA_BYTES_PER_SECOND
    ; "MEGA_BYTES_PER_SECOND", MEGA_BYTES_PER_SECOND
    ; "KILO_BYTES_PER_SECOND", KILO_BYTES_PER_SECOND
    ; "BYTES_PER_SECOND", BYTES_PER_SECOND
    ; "COUNT", COUNT
    ; "PERCENT", PERCENT
    ; "TERA_BITS", TERA_BITS
    ; "GIGA_BITS", GIGA_BITS
    ; "MEGA_BITS", MEGA_BITS
    ; "KILO_BITS", KILO_BITS
    ; "BITS", BITS
    ; "TERA_BYTES", TERA_BYTES
    ; "GIGA_BYTES", GIGA_BYTES
    ; "MEGA_BYTES", MEGA_BYTES
    ; "KILO_BYTES", KILO_BYTES
    ; "BYTES", BYTES
    ; "MILLI_SECONDS", MILLI_SECONDS
    ; "MICRO_SECONDS", MICRO_SECONDS
    ; "SECONDS", SECONDS
    ; "NONE", NONE
    ]

  let t_to_str =
    [ COUNT_PER_SECOND, "COUNT_PER_SECOND"
    ; TERA_BITS_PER_SECOND, "TERA_BITS_PER_SECOND"
    ; GIGA_BITS_PER_SECOND, "GIGA_BITS_PER_SECOND"
    ; MEGA_BITS_PER_SECOND, "MEGA_BITS_PER_SECOND"
    ; KILO_BITS_PER_SECOND, "KILO_BITS_PER_SECOND"
    ; BITS_PER_SECOND, "BITS_PER_SECOND"
    ; TERA_BYTES_PER_SECOND, "TERA_BYTES_PER_SECOND"
    ; GIGA_BYTES_PER_SECOND, "GIGA_BYTES_PER_SECOND"
    ; MEGA_BYTES_PER_SECOND, "MEGA_BYTES_PER_SECOND"
    ; KILO_BYTES_PER_SECOND, "KILO_BYTES_PER_SECOND"
    ; BYTES_PER_SECOND, "BYTES_PER_SECOND"
    ; COUNT, "COUNT"
    ; PERCENT, "PERCENT"
    ; TERA_BITS, "TERA_BITS"
    ; GIGA_BITS, "GIGA_BITS"
    ; MEGA_BITS, "MEGA_BITS"
    ; KILO_BITS, "KILO_BITS"
    ; BITS, "BITS"
    ; TERA_BYTES, "TERA_BYTES"
    ; GIGA_BYTES, "GIGA_BYTES"
    ; MEGA_BYTES, "MEGA_BYTES"
    ; KILO_BYTES, "KILO_BYTES"
    ; BYTES, "BYTES"
    ; MILLI_SECONDS, "MILLI_SECONDS"
    ; MICRO_SECONDS, "MICRO_SECONDS"
    ; SECONDS, "SECONDS"
    ; NONE, "NONE"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module EbsBlockDeviceConfig = struct
  type t =
    { volume_specification : VolumeSpecification.t
    ; volumes_per_instance : Integer.t option
    }

  let make ~volume_specification ?volumes_per_instance () =
    { volume_specification; volumes_per_instance }

  let parse xml =
    Some
      { volume_specification =
          Xml.required
            "VolumeSpecification"
            (Util.option_bind
               (Xml.member "VolumeSpecification" xml)
               VolumeSpecification.parse)
      ; volumes_per_instance =
          Util.option_bind (Xml.member "VolumesPerInstance" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.volumes_per_instance (fun f ->
               Query.Pair ("VolumesPerInstance", Integer.to_query f))
         ; Some
             (Query.Pair
                ( "VolumeSpecification"
                , VolumeSpecification.to_query v.volume_specification ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.volumes_per_instance (fun f ->
               "volumes_per_instance", Integer.to_json f)
         ; Some
             ("volume_specification", VolumeSpecification.to_json v.volume_specification)
         ])

  let of_json j =
    { volume_specification =
        VolumeSpecification.of_json
          (Util.of_option_exn (Json.lookup j "volume_specification"))
    ; volumes_per_instance =
        Util.option_map (Json.lookup j "volumes_per_instance") Integer.of_json
    }
end

module MarketType = struct
  type t =
    | ON_DEMAND
    | SPOT

  let str_to_t = [ "SPOT", SPOT; "ON_DEMAND", ON_DEMAND ]

  let t_to_str = [ SPOT, "SPOT"; ON_DEMAND, "ON_DEMAND" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module SimpleScalingPolicyConfiguration = struct
  type t =
    { adjustment_type : AdjustmentType.t option
    ; scaling_adjustment : Integer.t
    ; cool_down : Integer.t option
    }

  let make ?adjustment_type ~scaling_adjustment ?cool_down () =
    { adjustment_type; scaling_adjustment; cool_down }

  let parse xml =
    Some
      { adjustment_type =
          Util.option_bind (Xml.member "AdjustmentType" xml) AdjustmentType.parse
      ; scaling_adjustment =
          Xml.required
            "ScalingAdjustment"
            (Util.option_bind (Xml.member "ScalingAdjustment" xml) Integer.parse)
      ; cool_down = Util.option_bind (Xml.member "CoolDown" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.cool_down (fun f ->
               Query.Pair ("CoolDown", Integer.to_query f))
         ; Some (Query.Pair ("ScalingAdjustment", Integer.to_query v.scaling_adjustment))
         ; Util.option_map v.adjustment_type (fun f ->
               Query.Pair ("AdjustmentType", AdjustmentType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.cool_down (fun f -> "cool_down", Integer.to_json f)
         ; Some ("scaling_adjustment", Integer.to_json v.scaling_adjustment)
         ; Util.option_map v.adjustment_type (fun f ->
               "adjustment_type", AdjustmentType.to_json f)
         ])

  let of_json j =
    { adjustment_type =
        Util.option_map (Json.lookup j "adjustment_type") AdjustmentType.of_json
    ; scaling_adjustment =
        Integer.of_json (Util.of_option_exn (Json.lookup j "scaling_adjustment"))
    ; cool_down = Util.option_map (Json.lookup j "cool_down") Integer.of_json
    }
end

module CloudWatchAlarmDefinition = struct
  type t =
    { comparison_operator : ComparisonOperator.t
    ; evaluation_periods : Integer.t option
    ; metric_name : String.t
    ; namespace : String.t option
    ; period : Integer.t
    ; statistic : Statistic.t option
    ; threshold : Double.t
    ; unit : Unit.t option
    ; dimensions : MetricDimensionList.t
    }

  let make
      ~comparison_operator
      ?evaluation_periods
      ~metric_name
      ?namespace
      ~period
      ?statistic
      ~threshold
      ?unit
      ?(dimensions = [])
      () =
    { comparison_operator
    ; evaluation_periods
    ; metric_name
    ; namespace
    ; period
    ; statistic
    ; threshold
    ; unit
    ; dimensions
    }

  let parse xml =
    Some
      { comparison_operator =
          Xml.required
            "ComparisonOperator"
            (Util.option_bind
               (Xml.member "ComparisonOperator" xml)
               ComparisonOperator.parse)
      ; evaluation_periods =
          Util.option_bind (Xml.member "EvaluationPeriods" xml) Integer.parse
      ; metric_name =
          Xml.required
            "MetricName"
            (Util.option_bind (Xml.member "MetricName" xml) String.parse)
      ; namespace = Util.option_bind (Xml.member "Namespace" xml) String.parse
      ; period =
          Xml.required "Period" (Util.option_bind (Xml.member "Period" xml) Integer.parse)
      ; statistic = Util.option_bind (Xml.member "Statistic" xml) Statistic.parse
      ; threshold =
          Xml.required
            "Threshold"
            (Util.option_bind (Xml.member "Threshold" xml) Double.parse)
      ; unit = Util.option_bind (Xml.member "Unit" xml) Unit.parse
      ; dimensions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Dimensions" xml) MetricDimensionList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("Dimensions.member", MetricDimensionList.to_query v.dimensions))
         ; Util.option_map v.unit (fun f -> Query.Pair ("Unit", Unit.to_query f))
         ; Some (Query.Pair ("Threshold", Double.to_query v.threshold))
         ; Util.option_map v.statistic (fun f ->
               Query.Pair ("Statistic", Statistic.to_query f))
         ; Some (Query.Pair ("Period", Integer.to_query v.period))
         ; Util.option_map v.namespace (fun f ->
               Query.Pair ("Namespace", String.to_query f))
         ; Some (Query.Pair ("MetricName", String.to_query v.metric_name))
         ; Util.option_map v.evaluation_periods (fun f ->
               Query.Pair ("EvaluationPeriods", Integer.to_query f))
         ; Some
             (Query.Pair
                ("ComparisonOperator", ComparisonOperator.to_query v.comparison_operator))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("dimensions", MetricDimensionList.to_json v.dimensions)
         ; Util.option_map v.unit (fun f -> "unit", Unit.to_json f)
         ; Some ("threshold", Double.to_json v.threshold)
         ; Util.option_map v.statistic (fun f -> "statistic", Statistic.to_json f)
         ; Some ("period", Integer.to_json v.period)
         ; Util.option_map v.namespace (fun f -> "namespace", String.to_json f)
         ; Some ("metric_name", String.to_json v.metric_name)
         ; Util.option_map v.evaluation_periods (fun f ->
               "evaluation_periods", Integer.to_json f)
         ; Some ("comparison_operator", ComparisonOperator.to_json v.comparison_operator)
         ])

  let of_json j =
    { comparison_operator =
        ComparisonOperator.of_json
          (Util.of_option_exn (Json.lookup j "comparison_operator"))
    ; evaluation_periods =
        Util.option_map (Json.lookup j "evaluation_periods") Integer.of_json
    ; metric_name = String.of_json (Util.of_option_exn (Json.lookup j "metric_name"))
    ; namespace = Util.option_map (Json.lookup j "namespace") String.of_json
    ; period = Integer.of_json (Util.of_option_exn (Json.lookup j "period"))
    ; statistic = Util.option_map (Json.lookup j "statistic") Statistic.of_json
    ; threshold = Double.of_json (Util.of_option_exn (Json.lookup j "threshold"))
    ; unit = Util.option_map (Json.lookup j "unit") Unit.of_json
    ; dimensions =
        MetricDimensionList.of_json (Util.of_option_exn (Json.lookup j "dimensions"))
    }
end

module KeyValue = struct
  type t =
    { key : String.t option
    ; value : String.t option
    }

  let make ?key ?value () = { key; value }

  let parse xml =
    Some
      { key = Util.option_bind (Xml.member "Key" xml) String.parse
      ; value = Util.option_bind (Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> Query.Pair ("Value", String.to_query f))
         ; Util.option_map v.key (fun f -> Query.Pair ("Key", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> "value", String.to_json f)
         ; Util.option_map v.key (fun f -> "key", String.to_json f)
         ])

  let of_json j =
    { key = Util.option_map (Json.lookup j "key") String.of_json
    ; value = Util.option_map (Json.lookup j "value") String.of_json
    }
end

module StringMap = struct
  type t = (String.t, String.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v = Query.to_query_hashtbl String.to_string String.to_query v

  let to_json v =
    `Assoc
      (Hashtbl.fold (fun k v acc -> (String.to_string k, String.to_json v) :: acc) v [])

  let of_json j = Json.to_hashtbl String.of_string String.of_json j
end

module EbsBlockDeviceConfigList = struct
  type t = EbsBlockDeviceConfig.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map EbsBlockDeviceConfig.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list EbsBlockDeviceConfig.to_query v

  let to_json v = `List (List.map EbsBlockDeviceConfig.to_json v)

  let of_json j = Json.to_list EbsBlockDeviceConfig.of_json j
end

module ScalingAction = struct
  type t =
    { market : MarketType.t option
    ; simple_scaling_policy_configuration : SimpleScalingPolicyConfiguration.t
    }

  let make ?market ~simple_scaling_policy_configuration () =
    { market; simple_scaling_policy_configuration }

  let parse xml =
    Some
      { market = Util.option_bind (Xml.member "Market" xml) MarketType.parse
      ; simple_scaling_policy_configuration =
          Xml.required
            "SimpleScalingPolicyConfiguration"
            (Util.option_bind
               (Xml.member "SimpleScalingPolicyConfiguration" xml)
               SimpleScalingPolicyConfiguration.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "SimpleScalingPolicyConfiguration"
                , SimpleScalingPolicyConfiguration.to_query
                    v.simple_scaling_policy_configuration ))
         ; Util.option_map v.market (fun f ->
               Query.Pair ("Market", MarketType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "simple_scaling_policy_configuration"
             , SimpleScalingPolicyConfiguration.to_json
                 v.simple_scaling_policy_configuration )
         ; Util.option_map v.market (fun f -> "market", MarketType.to_json f)
         ])

  let of_json j =
    { market = Util.option_map (Json.lookup j "market") MarketType.of_json
    ; simple_scaling_policy_configuration =
        SimpleScalingPolicyConfiguration.of_json
          (Util.of_option_exn (Json.lookup j "simple_scaling_policy_configuration"))
    }
end

module ScalingTrigger = struct
  type t = { cloud_watch_alarm_definition : CloudWatchAlarmDefinition.t }

  let make ~cloud_watch_alarm_definition () = { cloud_watch_alarm_definition }

  let parse xml =
    Some
      { cloud_watch_alarm_definition =
          Xml.required
            "CloudWatchAlarmDefinition"
            (Util.option_bind
               (Xml.member "CloudWatchAlarmDefinition" xml)
               CloudWatchAlarmDefinition.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "CloudWatchAlarmDefinition"
                , CloudWatchAlarmDefinition.to_query v.cloud_watch_alarm_definition ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "cloud_watch_alarm_definition"
             , CloudWatchAlarmDefinition.to_json v.cloud_watch_alarm_definition )
         ])

  let of_json j =
    { cloud_watch_alarm_definition =
        CloudWatchAlarmDefinition.of_json
          (Util.of_option_exn (Json.lookup j "cloud_watch_alarm_definition"))
    }
end

module XmlStringList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module KeyValueList = struct
  type t = KeyValue.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map KeyValue.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list KeyValue.to_query v

  let to_json v = `List (List.map KeyValue.to_json v)

  let of_json j = Json.to_list KeyValue.of_json j
end

module SpotProvisioningTimeoutAction = struct
  type t =
    | SWITCH_TO_ON_DEMAND
    | TERMINATE_CLUSTER

  let str_to_t =
    [ "TERMINATE_CLUSTER", TERMINATE_CLUSTER; "SWITCH_TO_ON_DEMAND", SWITCH_TO_ON_DEMAND ]

  let t_to_str =
    [ TERMINATE_CLUSTER, "TERMINATE_CLUSTER"; SWITCH_TO_ON_DEMAND, "SWITCH_TO_ON_DEMAND" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ConfigurationList = struct
  type t = Configuration.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map Configuration.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Configuration.to_query v

  let to_json v = `List (List.map Configuration.to_json v)

  let of_json j = Json.to_list Configuration.of_json j
end

module Configuration = struct
  type t =
    { classification : String.t option
    ; configurations : ConfigurationList.t
    ; properties : StringMap.t option
    }

  let make ?classification ?(configurations = []) ?properties () =
    { classification; configurations; properties }

  let parse xml =
    Some
      { classification = Util.option_bind (Xml.member "Classification" xml) String.parse
      ; configurations =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Configurations" xml) ConfigurationList.parse)
      ; properties = Util.option_bind (Xml.member "Properties" xml) StringMap.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.properties (fun f ->
               Query.Pair ("Properties", StringMap.to_query f))
         ; Some
             (Query.Pair
                ("Configurations.member", ConfigurationList.to_query v.configurations))
         ; Util.option_map v.classification (fun f ->
               Query.Pair ("Classification", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.properties (fun f -> "properties", StringMap.to_json f)
         ; Some ("configurations", ConfigurationList.to_json v.configurations)
         ; Util.option_map v.classification (fun f -> "classification", String.to_json f)
         ])

  let of_json j =
    { classification = Util.option_map (Json.lookup j "classification") String.of_json
    ; configurations =
        ConfigurationList.of_json (Util.of_option_exn (Json.lookup j "configurations"))
    ; properties = Util.option_map (Json.lookup j "properties") StringMap.of_json
    }
end

module EbsConfiguration = struct
  type t =
    { ebs_block_device_configs : EbsBlockDeviceConfigList.t
    ; ebs_optimized : Boolean.t option
    }

  let make ?(ebs_block_device_configs = []) ?ebs_optimized () =
    { ebs_block_device_configs; ebs_optimized }

  let parse xml =
    Some
      { ebs_block_device_configs =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EbsBlockDeviceConfigs" xml)
               EbsBlockDeviceConfigList.parse)
      ; ebs_optimized = Util.option_bind (Xml.member "EbsOptimized" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.ebs_optimized (fun f ->
               Query.Pair ("EbsOptimized", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "EbsBlockDeviceConfigs.member"
                , EbsBlockDeviceConfigList.to_query v.ebs_block_device_configs ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.ebs_optimized (fun f -> "ebs_optimized", Boolean.to_json f)
         ; Some
             ( "ebs_block_device_configs"
             , EbsBlockDeviceConfigList.to_json v.ebs_block_device_configs )
         ])

  let of_json j =
    { ebs_block_device_configs =
        EbsBlockDeviceConfigList.of_json
          (Util.of_option_exn (Json.lookup j "ebs_block_device_configs"))
    ; ebs_optimized = Util.option_map (Json.lookup j "ebs_optimized") Boolean.of_json
    }
end

module ScalingRule = struct
  type t =
    { name : String.t
    ; description : String.t option
    ; action : ScalingAction.t
    ; trigger : ScalingTrigger.t
    }

  let make ~name ?description ~action ~trigger () = { name; description; action; trigger }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; description = Util.option_bind (Xml.member "Description" xml) String.parse
      ; action =
          Xml.required
            "Action"
            (Util.option_bind (Xml.member "Action" xml) ScalingAction.parse)
      ; trigger =
          Xml.required
            "Trigger"
            (Util.option_bind (Xml.member "Trigger" xml) ScalingTrigger.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Trigger", ScalingTrigger.to_query v.trigger))
         ; Some (Query.Pair ("Action", ScalingAction.to_query v.action))
         ; Util.option_map v.description (fun f ->
               Query.Pair ("Description", String.to_query f))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("trigger", ScalingTrigger.to_json v.trigger)
         ; Some ("action", ScalingAction.to_json v.action)
         ; Util.option_map v.description (fun f -> "description", String.to_json f)
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; description = Util.option_map (Json.lookup j "description") String.of_json
    ; action = ScalingAction.of_json (Util.of_option_exn (Json.lookup j "action"))
    ; trigger = ScalingTrigger.of_json (Util.of_option_exn (Json.lookup j "trigger"))
    }
end

module EbsBlockDevice = struct
  type t =
    { volume_specification : VolumeSpecification.t option
    ; device : String.t option
    }

  let make ?volume_specification ?device () = { volume_specification; device }

  let parse xml =
    Some
      { volume_specification =
          Util.option_bind
            (Xml.member "VolumeSpecification" xml)
            VolumeSpecification.parse
      ; device = Util.option_bind (Xml.member "Device" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.device (fun f -> Query.Pair ("Device", String.to_query f))
         ; Util.option_map v.volume_specification (fun f ->
               Query.Pair ("VolumeSpecification", VolumeSpecification.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.device (fun f -> "device", String.to_json f)
         ; Util.option_map v.volume_specification (fun f ->
               "volume_specification", VolumeSpecification.to_json f)
         ])

  let of_json j =
    { volume_specification =
        Util.option_map (Json.lookup j "volume_specification") VolumeSpecification.of_json
    ; device = Util.option_map (Json.lookup j "device") String.of_json
    }
end

module AutoScalingPolicyStateChangeReasonCode = struct
  type t =
    | USER_REQUEST
    | PROVISION_FAILURE
    | CLEANUP_FAILURE

  let str_to_t =
    [ "CLEANUP_FAILURE", CLEANUP_FAILURE
    ; "PROVISION_FAILURE", PROVISION_FAILURE
    ; "USER_REQUEST", USER_REQUEST
    ]

  let t_to_str =
    [ CLEANUP_FAILURE, "CLEANUP_FAILURE"
    ; PROVISION_FAILURE, "PROVISION_FAILURE"
    ; USER_REQUEST, "USER_REQUEST"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ScriptBootstrapActionConfig = struct
  type t =
    { path : String.t
    ; args : XmlStringList.t
    }

  let make ~path ?(args = []) () = { path; args }

  let parse xml =
    Some
      { path = Xml.required "Path" (Util.option_bind (Xml.member "Path" xml) String.parse)
      ; args =
          Util.of_option [] (Util.option_bind (Xml.member "Args" xml) XmlStringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Args.member", XmlStringList.to_query v.args))
         ; Some (Query.Pair ("Path", String.to_query v.path))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("args", XmlStringList.to_json v.args)
         ; Some ("path", String.to_json v.path)
         ])

  let of_json j =
    { path = String.of_json (Util.of_option_exn (Json.lookup j "path"))
    ; args = XmlStringList.of_json (Util.of_option_exn (Json.lookup j "args"))
    }
end

module InstanceGroupState = struct
  type t =
    | PROVISIONING
    | BOOTSTRAPPING
    | RUNNING
    | RESIZING
    | SUSPENDED
    | TERMINATING
    | TERMINATED
    | ARRESTED
    | SHUTTING_DOWN
    | ENDED

  let str_to_t =
    [ "ENDED", ENDED
    ; "SHUTTING_DOWN", SHUTTING_DOWN
    ; "ARRESTED", ARRESTED
    ; "TERMINATED", TERMINATED
    ; "TERMINATING", TERMINATING
    ; "SUSPENDED", SUSPENDED
    ; "RESIZING", RESIZING
    ; "RUNNING", RUNNING
    ; "BOOTSTRAPPING", BOOTSTRAPPING
    ; "PROVISIONING", PROVISIONING
    ]

  let t_to_str =
    [ ENDED, "ENDED"
    ; SHUTTING_DOWN, "SHUTTING_DOWN"
    ; ARRESTED, "ARRESTED"
    ; TERMINATED, "TERMINATED"
    ; TERMINATING, "TERMINATING"
    ; SUSPENDED, "SUSPENDED"
    ; RESIZING, "RESIZING"
    ; RUNNING, "RUNNING"
    ; BOOTSTRAPPING, "BOOTSTRAPPING"
    ; PROVISIONING, "PROVISIONING"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module InstanceRoleType = struct
  type t =
    | MASTER
    | CORE
    | TASK

  let str_to_t = [ "TASK", TASK; "CORE", CORE; "MASTER", MASTER ]

  let t_to_str = [ TASK, "TASK"; CORE, "CORE"; MASTER, "MASTER" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ActionOnFailure = struct
  type t =
    | TERMINATE_JOB_FLOW
    | TERMINATE_CLUSTER
    | CANCEL_AND_WAIT
    | CONTINUE

  let str_to_t =
    [ "CONTINUE", CONTINUE
    ; "CANCEL_AND_WAIT", CANCEL_AND_WAIT
    ; "TERMINATE_CLUSTER", TERMINATE_CLUSTER
    ; "TERMINATE_JOB_FLOW", TERMINATE_JOB_FLOW
    ]

  let t_to_str =
    [ CONTINUE, "CONTINUE"
    ; CANCEL_AND_WAIT, "CANCEL_AND_WAIT"
    ; TERMINATE_CLUSTER, "TERMINATE_CLUSTER"
    ; TERMINATE_JOB_FLOW, "TERMINATE_JOB_FLOW"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module HadoopJarStepConfig = struct
  type t =
    { properties : KeyValueList.t
    ; jar : String.t
    ; main_class : String.t option
    ; args : XmlStringList.t
    }

  let make ?(properties = []) ~jar ?main_class ?(args = []) () =
    { properties; jar; main_class; args }

  let parse xml =
    Some
      { properties =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Properties" xml) KeyValueList.parse)
      ; jar = Xml.required "Jar" (Util.option_bind (Xml.member "Jar" xml) String.parse)
      ; main_class = Util.option_bind (Xml.member "MainClass" xml) String.parse
      ; args =
          Util.of_option [] (Util.option_bind (Xml.member "Args" xml) XmlStringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Args.member", XmlStringList.to_query v.args))
         ; Util.option_map v.main_class (fun f ->
               Query.Pair ("MainClass", String.to_query f))
         ; Some (Query.Pair ("Jar", String.to_query v.jar))
         ; Some (Query.Pair ("Properties.member", KeyValueList.to_query v.properties))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("args", XmlStringList.to_json v.args)
         ; Util.option_map v.main_class (fun f -> "main_class", String.to_json f)
         ; Some ("jar", String.to_json v.jar)
         ; Some ("properties", KeyValueList.to_json v.properties)
         ])

  let of_json j =
    { properties = KeyValueList.of_json (Util.of_option_exn (Json.lookup j "properties"))
    ; jar = String.of_json (Util.of_option_exn (Json.lookup j "jar"))
    ; main_class = Util.option_map (Json.lookup j "main_class") String.of_json
    ; args = XmlStringList.of_json (Util.of_option_exn (Json.lookup j "args"))
    }
end

module StepExecutionState = struct
  type t =
    | PENDING
    | RUNNING
    | CONTINUE
    | COMPLETED
    | CANCELLED
    | FAILED
    | INTERRUPTED

  let str_to_t =
    [ "INTERRUPTED", INTERRUPTED
    ; "FAILED", FAILED
    ; "CANCELLED", CANCELLED
    ; "COMPLETED", COMPLETED
    ; "CONTINUE", CONTINUE
    ; "RUNNING", RUNNING
    ; "PENDING", PENDING
    ]

  let t_to_str =
    [ INTERRUPTED, "INTERRUPTED"
    ; FAILED, "FAILED"
    ; CANCELLED, "CANCELLED"
    ; COMPLETED, "COMPLETED"
    ; CONTINUE, "CONTINUE"
    ; RUNNING, "RUNNING"
    ; PENDING, "PENDING"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module SpotProvisioningSpecification = struct
  type t =
    { timeout_duration_minutes : Integer.t
    ; timeout_action : SpotProvisioningTimeoutAction.t
    ; block_duration_minutes : Integer.t option
    }

  let make ~timeout_duration_minutes ~timeout_action ?block_duration_minutes () =
    { timeout_duration_minutes; timeout_action; block_duration_minutes }

  let parse xml =
    Some
      { timeout_duration_minutes =
          Xml.required
            "TimeoutDurationMinutes"
            (Util.option_bind (Xml.member "TimeoutDurationMinutes" xml) Integer.parse)
      ; timeout_action =
          Xml.required
            "TimeoutAction"
            (Util.option_bind
               (Xml.member "TimeoutAction" xml)
               SpotProvisioningTimeoutAction.parse)
      ; block_duration_minutes =
          Util.option_bind (Xml.member "BlockDurationMinutes" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.block_duration_minutes (fun f ->
               Query.Pair ("BlockDurationMinutes", Integer.to_query f))
         ; Some
             (Query.Pair
                ("TimeoutAction", SpotProvisioningTimeoutAction.to_query v.timeout_action))
         ; Some
             (Query.Pair
                ("TimeoutDurationMinutes", Integer.to_query v.timeout_duration_minutes))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.block_duration_minutes (fun f ->
               "block_duration_minutes", Integer.to_json f)
         ; Some ("timeout_action", SpotProvisioningTimeoutAction.to_json v.timeout_action)
         ; Some ("timeout_duration_minutes", Integer.to_json v.timeout_duration_minutes)
         ])

  let of_json j =
    { timeout_duration_minutes =
        Integer.of_json (Util.of_option_exn (Json.lookup j "timeout_duration_minutes"))
    ; timeout_action =
        SpotProvisioningTimeoutAction.of_json
          (Util.of_option_exn (Json.lookup j "timeout_action"))
    ; block_duration_minutes =
        Util.option_map (Json.lookup j "block_duration_minutes") Integer.of_json
    }
end

module InstanceTypeConfig = struct
  type t =
    { instance_type : String.t
    ; weighted_capacity : Integer.t option
    ; bid_price : String.t option
    ; bid_price_as_percentage_of_on_demand_price : Double.t option
    ; ebs_configuration : EbsConfiguration.t option
    ; configurations : ConfigurationList.t
    }

  let make
      ~instance_type
      ?weighted_capacity
      ?bid_price
      ?bid_price_as_percentage_of_on_demand_price
      ?ebs_configuration
      ?(configurations = [])
      () =
    { instance_type
    ; weighted_capacity
    ; bid_price
    ; bid_price_as_percentage_of_on_demand_price
    ; ebs_configuration
    ; configurations
    }

  let parse xml =
    Some
      { instance_type =
          Xml.required
            "InstanceType"
            (Util.option_bind (Xml.member "InstanceType" xml) String.parse)
      ; weighted_capacity =
          Util.option_bind (Xml.member "WeightedCapacity" xml) Integer.parse
      ; bid_price = Util.option_bind (Xml.member "BidPrice" xml) String.parse
      ; bid_price_as_percentage_of_on_demand_price =
          Util.option_bind
            (Xml.member "BidPriceAsPercentageOfOnDemandPrice" xml)
            Double.parse
      ; ebs_configuration =
          Util.option_bind (Xml.member "EbsConfiguration" xml) EbsConfiguration.parse
      ; configurations =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Configurations" xml) ConfigurationList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("Configurations.member", ConfigurationList.to_query v.configurations))
         ; Util.option_map v.ebs_configuration (fun f ->
               Query.Pair ("EbsConfiguration", EbsConfiguration.to_query f))
         ; Util.option_map v.bid_price_as_percentage_of_on_demand_price (fun f ->
               Query.Pair ("BidPriceAsPercentageOfOnDemandPrice", Double.to_query f))
         ; Util.option_map v.bid_price (fun f ->
               Query.Pair ("BidPrice", String.to_query f))
         ; Util.option_map v.weighted_capacity (fun f ->
               Query.Pair ("WeightedCapacity", Integer.to_query f))
         ; Some (Query.Pair ("InstanceType", String.to_query v.instance_type))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("configurations", ConfigurationList.to_json v.configurations)
         ; Util.option_map v.ebs_configuration (fun f ->
               "ebs_configuration", EbsConfiguration.to_json f)
         ; Util.option_map v.bid_price_as_percentage_of_on_demand_price (fun f ->
               "bid_price_as_percentage_of_on_demand_price", Double.to_json f)
         ; Util.option_map v.bid_price (fun f -> "bid_price", String.to_json f)
         ; Util.option_map v.weighted_capacity (fun f ->
               "weighted_capacity", Integer.to_json f)
         ; Some ("instance_type", String.to_json v.instance_type)
         ])

  let of_json j =
    { instance_type = String.of_json (Util.of_option_exn (Json.lookup j "instance_type"))
    ; weighted_capacity =
        Util.option_map (Json.lookup j "weighted_capacity") Integer.of_json
    ; bid_price = Util.option_map (Json.lookup j "bid_price") String.of_json
    ; bid_price_as_percentage_of_on_demand_price =
        Util.option_map
          (Json.lookup j "bid_price_as_percentage_of_on_demand_price")
          Double.of_json
    ; ebs_configuration =
        Util.option_map (Json.lookup j "ebs_configuration") EbsConfiguration.of_json
    ; configurations =
        ConfigurationList.of_json (Util.of_option_exn (Json.lookup j "configurations"))
    }
end

module ScalingConstraints = struct
  type t =
    { min_capacity : Integer.t
    ; max_capacity : Integer.t
    }

  let make ~min_capacity ~max_capacity () = { min_capacity; max_capacity }

  let parse xml =
    Some
      { min_capacity =
          Xml.required
            "MinCapacity"
            (Util.option_bind (Xml.member "MinCapacity" xml) Integer.parse)
      ; max_capacity =
          Xml.required
            "MaxCapacity"
            (Util.option_bind (Xml.member "MaxCapacity" xml) Integer.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("MaxCapacity", Integer.to_query v.max_capacity))
         ; Some (Query.Pair ("MinCapacity", Integer.to_query v.min_capacity))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("max_capacity", Integer.to_json v.max_capacity)
         ; Some ("min_capacity", Integer.to_json v.min_capacity)
         ])

  let of_json j =
    { min_capacity = Integer.of_json (Util.of_option_exn (Json.lookup j "min_capacity"))
    ; max_capacity = Integer.of_json (Util.of_option_exn (Json.lookup j "max_capacity"))
    }
end

module ScalingRuleList = struct
  type t = ScalingRule.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map ScalingRule.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list ScalingRule.to_query v

  let to_json v = `List (List.map ScalingRule.to_json v)

  let of_json j = Json.to_list ScalingRule.of_json j
end

module InstanceFleetStateChangeReasonCode = struct
  type t =
    | INTERNAL_ERROR
    | VALIDATION_ERROR
    | INSTANCE_FAILURE
    | CLUSTER_TERMINATED

  let str_to_t =
    [ "CLUSTER_TERMINATED", CLUSTER_TERMINATED
    ; "INSTANCE_FAILURE", INSTANCE_FAILURE
    ; "VALIDATION_ERROR", VALIDATION_ERROR
    ; "INTERNAL_ERROR", INTERNAL_ERROR
    ]

  let t_to_str =
    [ CLUSTER_TERMINATED, "CLUSTER_TERMINATED"
    ; INSTANCE_FAILURE, "INSTANCE_FAILURE"
    ; VALIDATION_ERROR, "VALIDATION_ERROR"
    ; INTERNAL_ERROR, "INTERNAL_ERROR"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module EbsBlockDeviceList = struct
  type t = EbsBlockDevice.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map EbsBlockDevice.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list EbsBlockDevice.to_query v

  let to_json v = `List (List.map EbsBlockDevice.to_json v)

  let of_json j = Json.to_list EbsBlockDevice.of_json j
end

module StepStateChangeReasonCode = struct
  type t = NONE

  let str_to_t = [ "NONE", NONE ]

  let t_to_str = [ NONE, "NONE" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module EC2InstanceIdsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module AutoScalingPolicyState = struct
  type t =
    | PENDING
    | ATTACHING
    | ATTACHED
    | DETACHING
    | DETACHED
    | FAILED

  let str_to_t =
    [ "FAILED", FAILED
    ; "DETACHED", DETACHED
    ; "DETACHING", DETACHING
    ; "ATTACHED", ATTACHED
    ; "ATTACHING", ATTACHING
    ; "PENDING", PENDING
    ]

  let t_to_str =
    [ FAILED, "FAILED"
    ; DETACHED, "DETACHED"
    ; DETACHING, "DETACHING"
    ; ATTACHED, "ATTACHED"
    ; ATTACHING, "ATTACHING"
    ; PENDING, "PENDING"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module AutoScalingPolicyStateChangeReason = struct
  type t =
    { code : AutoScalingPolicyStateChangeReasonCode.t option
    ; message : String.t option
    }

  let make ?code ?message () = { code; message }

  let parse xml =
    Some
      { code =
          Util.option_bind
            (Xml.member "Code" xml)
            AutoScalingPolicyStateChangeReasonCode.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.code (fun f ->
               Query.Pair ("Code", AutoScalingPolicyStateChangeReasonCode.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.code (fun f ->
               "code", AutoScalingPolicyStateChangeReasonCode.to_json f)
         ])

  let of_json j =
    { code =
        Util.option_map
          (Json.lookup j "code")
          AutoScalingPolicyStateChangeReasonCode.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    }
end

module InstanceGroupStateChangeReasonCode = struct
  type t =
    | INTERNAL_ERROR
    | VALIDATION_ERROR
    | INSTANCE_FAILURE
    | CLUSTER_TERMINATED

  let str_to_t =
    [ "CLUSTER_TERMINATED", CLUSTER_TERMINATED
    ; "INSTANCE_FAILURE", INSTANCE_FAILURE
    ; "VALIDATION_ERROR", VALIDATION_ERROR
    ; "INTERNAL_ERROR", INTERNAL_ERROR
    ]

  let t_to_str =
    [ CLUSTER_TERMINATED, "CLUSTER_TERMINATED"
    ; INSTANCE_FAILURE, "INSTANCE_FAILURE"
    ; VALIDATION_ERROR, "VALIDATION_ERROR"
    ; INTERNAL_ERROR, "INTERNAL_ERROR"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module InstanceStateChangeReasonCode = struct
  type t =
    | INTERNAL_ERROR
    | VALIDATION_ERROR
    | INSTANCE_FAILURE
    | BOOTSTRAP_FAILURE
    | CLUSTER_TERMINATED

  let str_to_t =
    [ "CLUSTER_TERMINATED", CLUSTER_TERMINATED
    ; "BOOTSTRAP_FAILURE", BOOTSTRAP_FAILURE
    ; "INSTANCE_FAILURE", INSTANCE_FAILURE
    ; "VALIDATION_ERROR", VALIDATION_ERROR
    ; "INTERNAL_ERROR", INTERNAL_ERROR
    ]

  let t_to_str =
    [ CLUSTER_TERMINATED, "CLUSTER_TERMINATED"
    ; BOOTSTRAP_FAILURE, "BOOTSTRAP_FAILURE"
    ; INSTANCE_FAILURE, "INSTANCE_FAILURE"
    ; VALIDATION_ERROR, "VALIDATION_ERROR"
    ; INTERNAL_ERROR, "INTERNAL_ERROR"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ClusterStateChangeReasonCode = struct
  type t =
    | INTERNAL_ERROR
    | VALIDATION_ERROR
    | INSTANCE_FAILURE
    | INSTANCE_FLEET_TIMEOUT
    | BOOTSTRAP_FAILURE
    | USER_REQUEST
    | STEP_FAILURE
    | ALL_STEPS_COMPLETED

  let str_to_t =
    [ "ALL_STEPS_COMPLETED", ALL_STEPS_COMPLETED
    ; "STEP_FAILURE", STEP_FAILURE
    ; "USER_REQUEST", USER_REQUEST
    ; "BOOTSTRAP_FAILURE", BOOTSTRAP_FAILURE
    ; "INSTANCE_FLEET_TIMEOUT", INSTANCE_FLEET_TIMEOUT
    ; "INSTANCE_FAILURE", INSTANCE_FAILURE
    ; "VALIDATION_ERROR", VALIDATION_ERROR
    ; "INTERNAL_ERROR", INTERNAL_ERROR
    ]

  let t_to_str =
    [ ALL_STEPS_COMPLETED, "ALL_STEPS_COMPLETED"
    ; STEP_FAILURE, "STEP_FAILURE"
    ; USER_REQUEST, "USER_REQUEST"
    ; BOOTSTRAP_FAILURE, "BOOTSTRAP_FAILURE"
    ; INSTANCE_FLEET_TIMEOUT, "INSTANCE_FLEET_TIMEOUT"
    ; INSTANCE_FAILURE, "INSTANCE_FAILURE"
    ; VALIDATION_ERROR, "VALIDATION_ERROR"
    ; INTERNAL_ERROR, "INTERNAL_ERROR"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module BootstrapActionConfig = struct
  type t =
    { name : String.t
    ; script_bootstrap_action : ScriptBootstrapActionConfig.t
    }

  let make ~name ~script_bootstrap_action () = { name; script_bootstrap_action }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; script_bootstrap_action =
          Xml.required
            "ScriptBootstrapAction"
            (Util.option_bind
               (Xml.member "ScriptBootstrapAction" xml)
               ScriptBootstrapActionConfig.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "ScriptBootstrapAction"
                , ScriptBootstrapActionConfig.to_query v.script_bootstrap_action ))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "script_bootstrap_action"
             , ScriptBootstrapActionConfig.to_json v.script_bootstrap_action )
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; script_bootstrap_action =
        ScriptBootstrapActionConfig.of_json
          (Util.of_option_exn (Json.lookup j "script_bootstrap_action"))
    }
end

module InstanceGroupDetail = struct
  type t =
    { instance_group_id : String.t option
    ; name : String.t option
    ; market : MarketType.t
    ; instance_role : InstanceRoleType.t
    ; bid_price : String.t option
    ; instance_type : String.t
    ; instance_request_count : Integer.t
    ; instance_running_count : Integer.t
    ; state : InstanceGroupState.t
    ; last_state_change_reason : String.t option
    ; creation_date_time : DateTime.t
    ; start_date_time : DateTime.t option
    ; ready_date_time : DateTime.t option
    ; end_date_time : DateTime.t option
    }

  let make
      ?instance_group_id
      ?name
      ~market
      ~instance_role
      ?bid_price
      ~instance_type
      ~instance_request_count
      ~instance_running_count
      ~state
      ?last_state_change_reason
      ~creation_date_time
      ?start_date_time
      ?ready_date_time
      ?end_date_time
      () =
    { instance_group_id
    ; name
    ; market
    ; instance_role
    ; bid_price
    ; instance_type
    ; instance_request_count
    ; instance_running_count
    ; state
    ; last_state_change_reason
    ; creation_date_time
    ; start_date_time
    ; ready_date_time
    ; end_date_time
    }

  let parse xml =
    Some
      { instance_group_id =
          Util.option_bind (Xml.member "InstanceGroupId" xml) String.parse
      ; name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; market =
          Xml.required
            "Market"
            (Util.option_bind (Xml.member "Market" xml) MarketType.parse)
      ; instance_role =
          Xml.required
            "InstanceRole"
            (Util.option_bind (Xml.member "InstanceRole" xml) InstanceRoleType.parse)
      ; bid_price = Util.option_bind (Xml.member "BidPrice" xml) String.parse
      ; instance_type =
          Xml.required
            "InstanceType"
            (Util.option_bind (Xml.member "InstanceType" xml) String.parse)
      ; instance_request_count =
          Xml.required
            "InstanceRequestCount"
            (Util.option_bind (Xml.member "InstanceRequestCount" xml) Integer.parse)
      ; instance_running_count =
          Xml.required
            "InstanceRunningCount"
            (Util.option_bind (Xml.member "InstanceRunningCount" xml) Integer.parse)
      ; state =
          Xml.required
            "State"
            (Util.option_bind (Xml.member "State" xml) InstanceGroupState.parse)
      ; last_state_change_reason =
          Util.option_bind (Xml.member "LastStateChangeReason" xml) String.parse
      ; creation_date_time =
          Xml.required
            "CreationDateTime"
            (Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse)
      ; start_date_time = Util.option_bind (Xml.member "StartDateTime" xml) DateTime.parse
      ; ready_date_time = Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse
      ; end_date_time = Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f ->
               Query.Pair ("EndDateTime", DateTime.to_query f))
         ; Util.option_map v.ready_date_time (fun f ->
               Query.Pair ("ReadyDateTime", DateTime.to_query f))
         ; Util.option_map v.start_date_time (fun f ->
               Query.Pair ("StartDateTime", DateTime.to_query f))
         ; Some (Query.Pair ("CreationDateTime", DateTime.to_query v.creation_date_time))
         ; Util.option_map v.last_state_change_reason (fun f ->
               Query.Pair ("LastStateChangeReason", String.to_query f))
         ; Some (Query.Pair ("State", InstanceGroupState.to_query v.state))
         ; Some
             (Query.Pair
                ("InstanceRunningCount", Integer.to_query v.instance_running_count))
         ; Some
             (Query.Pair
                ("InstanceRequestCount", Integer.to_query v.instance_request_count))
         ; Some (Query.Pair ("InstanceType", String.to_query v.instance_type))
         ; Util.option_map v.bid_price (fun f ->
               Query.Pair ("BidPrice", String.to_query f))
         ; Some (Query.Pair ("InstanceRole", InstanceRoleType.to_query v.instance_role))
         ; Some (Query.Pair ("Market", MarketType.to_query v.market))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ; Util.option_map v.instance_group_id (fun f ->
               Query.Pair ("InstanceGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f -> "end_date_time", DateTime.to_json f)
         ; Util.option_map v.ready_date_time (fun f ->
               "ready_date_time", DateTime.to_json f)
         ; Util.option_map v.start_date_time (fun f ->
               "start_date_time", DateTime.to_json f)
         ; Some ("creation_date_time", DateTime.to_json v.creation_date_time)
         ; Util.option_map v.last_state_change_reason (fun f ->
               "last_state_change_reason", String.to_json f)
         ; Some ("state", InstanceGroupState.to_json v.state)
         ; Some ("instance_running_count", Integer.to_json v.instance_running_count)
         ; Some ("instance_request_count", Integer.to_json v.instance_request_count)
         ; Some ("instance_type", String.to_json v.instance_type)
         ; Util.option_map v.bid_price (fun f -> "bid_price", String.to_json f)
         ; Some ("instance_role", InstanceRoleType.to_json v.instance_role)
         ; Some ("market", MarketType.to_json v.market)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ; Util.option_map v.instance_group_id (fun f ->
               "instance_group_id", String.to_json f)
         ])

  let of_json j =
    { instance_group_id =
        Util.option_map (Json.lookup j "instance_group_id") String.of_json
    ; name = Util.option_map (Json.lookup j "name") String.of_json
    ; market = MarketType.of_json (Util.of_option_exn (Json.lookup j "market"))
    ; instance_role =
        InstanceRoleType.of_json (Util.of_option_exn (Json.lookup j "instance_role"))
    ; bid_price = Util.option_map (Json.lookup j "bid_price") String.of_json
    ; instance_type = String.of_json (Util.of_option_exn (Json.lookup j "instance_type"))
    ; instance_request_count =
        Integer.of_json (Util.of_option_exn (Json.lookup j "instance_request_count"))
    ; instance_running_count =
        Integer.of_json (Util.of_option_exn (Json.lookup j "instance_running_count"))
    ; state = InstanceGroupState.of_json (Util.of_option_exn (Json.lookup j "state"))
    ; last_state_change_reason =
        Util.option_map (Json.lookup j "last_state_change_reason") String.of_json
    ; creation_date_time =
        DateTime.of_json (Util.of_option_exn (Json.lookup j "creation_date_time"))
    ; start_date_time = Util.option_map (Json.lookup j "start_date_time") DateTime.of_json
    ; ready_date_time = Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json
    ; end_date_time = Util.option_map (Json.lookup j "end_date_time") DateTime.of_json
    }
end

module XmlStringMaxLen256List = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module StepConfig = struct
  type t =
    { name : String.t
    ; action_on_failure : ActionOnFailure.t option
    ; hadoop_jar_step : HadoopJarStepConfig.t
    }

  let make ~name ?action_on_failure ~hadoop_jar_step () =
    { name; action_on_failure; hadoop_jar_step }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; action_on_failure =
          Util.option_bind (Xml.member "ActionOnFailure" xml) ActionOnFailure.parse
      ; hadoop_jar_step =
          Xml.required
            "HadoopJarStep"
            (Util.option_bind (Xml.member "HadoopJarStep" xml) HadoopJarStepConfig.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("HadoopJarStep", HadoopJarStepConfig.to_query v.hadoop_jar_step))
         ; Util.option_map v.action_on_failure (fun f ->
               Query.Pair ("ActionOnFailure", ActionOnFailure.to_query f))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("hadoop_jar_step", HadoopJarStepConfig.to_json v.hadoop_jar_step)
         ; Util.option_map v.action_on_failure (fun f ->
               "action_on_failure", ActionOnFailure.to_json f)
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; action_on_failure =
        Util.option_map (Json.lookup j "action_on_failure") ActionOnFailure.of_json
    ; hadoop_jar_step =
        HadoopJarStepConfig.of_json (Util.of_option_exn (Json.lookup j "hadoop_jar_step"))
    }
end

module StepExecutionStatusDetail = struct
  type t =
    { state : StepExecutionState.t
    ; creation_date_time : DateTime.t
    ; start_date_time : DateTime.t option
    ; end_date_time : DateTime.t option
    ; last_state_change_reason : String.t option
    }

  let make
      ~state
      ~creation_date_time
      ?start_date_time
      ?end_date_time
      ?last_state_change_reason
      () =
    { state
    ; creation_date_time
    ; start_date_time
    ; end_date_time
    ; last_state_change_reason
    }

  let parse xml =
    Some
      { state =
          Xml.required
            "State"
            (Util.option_bind (Xml.member "State" xml) StepExecutionState.parse)
      ; creation_date_time =
          Xml.required
            "CreationDateTime"
            (Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse)
      ; start_date_time = Util.option_bind (Xml.member "StartDateTime" xml) DateTime.parse
      ; end_date_time = Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse
      ; last_state_change_reason =
          Util.option_bind (Xml.member "LastStateChangeReason" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.last_state_change_reason (fun f ->
               Query.Pair ("LastStateChangeReason", String.to_query f))
         ; Util.option_map v.end_date_time (fun f ->
               Query.Pair ("EndDateTime", DateTime.to_query f))
         ; Util.option_map v.start_date_time (fun f ->
               Query.Pair ("StartDateTime", DateTime.to_query f))
         ; Some (Query.Pair ("CreationDateTime", DateTime.to_query v.creation_date_time))
         ; Some (Query.Pair ("State", StepExecutionState.to_query v.state))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.last_state_change_reason (fun f ->
               "last_state_change_reason", String.to_json f)
         ; Util.option_map v.end_date_time (fun f -> "end_date_time", DateTime.to_json f)
         ; Util.option_map v.start_date_time (fun f ->
               "start_date_time", DateTime.to_json f)
         ; Some ("creation_date_time", DateTime.to_json v.creation_date_time)
         ; Some ("state", StepExecutionState.to_json v.state)
         ])

  let of_json j =
    { state = StepExecutionState.of_json (Util.of_option_exn (Json.lookup j "state"))
    ; creation_date_time =
        DateTime.of_json (Util.of_option_exn (Json.lookup j "creation_date_time"))
    ; start_date_time = Util.option_map (Json.lookup j "start_date_time") DateTime.of_json
    ; end_date_time = Util.option_map (Json.lookup j "end_date_time") DateTime.of_json
    ; last_state_change_reason =
        Util.option_map (Json.lookup j "last_state_change_reason") String.of_json
    }
end

module StringList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module InstanceFleetProvisioningSpecifications = struct
  type t = { spot_specification : SpotProvisioningSpecification.t }

  let make ~spot_specification () = { spot_specification }

  let parse xml =
    Some
      { spot_specification =
          Xml.required
            "SpotSpecification"
            (Util.option_bind
               (Xml.member "SpotSpecification" xml)
               SpotProvisioningSpecification.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "SpotSpecification"
                , SpotProvisioningSpecification.to_query v.spot_specification ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "spot_specification"
             , SpotProvisioningSpecification.to_json v.spot_specification )
         ])

  let of_json j =
    { spot_specification =
        SpotProvisioningSpecification.of_json
          (Util.of_option_exn (Json.lookup j "spot_specification"))
    }
end

module InstanceFleetType = struct
  type t =
    | MASTER
    | CORE
    | TASK

  let str_to_t = [ "TASK", TASK; "CORE", CORE; "MASTER", MASTER ]

  let t_to_str = [ TASK, "TASK"; CORE, "CORE"; MASTER, "MASTER" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module InstanceTypeConfigList = struct
  type t = InstanceTypeConfig.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceTypeConfig.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceTypeConfig.to_query v

  let to_json v = `List (List.map InstanceTypeConfig.to_json v)

  let of_json j = Json.to_list InstanceTypeConfig.of_json j
end

module AutoScalingPolicy = struct
  type t =
    { constraints : ScalingConstraints.t
    ; rules : ScalingRuleList.t
    }

  let make ~constraints ~rules () = { constraints; rules }

  let parse xml =
    Some
      { constraints =
          Xml.required
            "Constraints"
            (Util.option_bind (Xml.member "Constraints" xml) ScalingConstraints.parse)
      ; rules =
          Xml.required
            "Rules"
            (Util.option_bind (Xml.member "Rules" xml) ScalingRuleList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Rules.member", ScalingRuleList.to_query v.rules))
         ; Some (Query.Pair ("Constraints", ScalingConstraints.to_query v.constraints))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("rules", ScalingRuleList.to_json v.rules)
         ; Some ("constraints", ScalingConstraints.to_json v.constraints)
         ])

  let of_json j =
    { constraints =
        ScalingConstraints.of_json (Util.of_option_exn (Json.lookup j "constraints"))
    ; rules = ScalingRuleList.of_json (Util.of_option_exn (Json.lookup j "rules"))
    }
end

module InstanceFleetState = struct
  type t =
    | PROVISIONING
    | BOOTSTRAPPING
    | RUNNING
    | RESIZING
    | SUSPENDED
    | TERMINATING
    | TERMINATED

  let str_to_t =
    [ "TERMINATED", TERMINATED
    ; "TERMINATING", TERMINATING
    ; "SUSPENDED", SUSPENDED
    ; "RESIZING", RESIZING
    ; "RUNNING", RUNNING
    ; "BOOTSTRAPPING", BOOTSTRAPPING
    ; "PROVISIONING", PROVISIONING
    ]

  let t_to_str =
    [ TERMINATED, "TERMINATED"
    ; TERMINATING, "TERMINATING"
    ; SUSPENDED, "SUSPENDED"
    ; RESIZING, "RESIZING"
    ; RUNNING, "RUNNING"
    ; BOOTSTRAPPING, "BOOTSTRAPPING"
    ; PROVISIONING, "PROVISIONING"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module InstanceFleetStateChangeReason = struct
  type t =
    { code : InstanceFleetStateChangeReasonCode.t option
    ; message : String.t option
    }

  let make ?code ?message () = { code; message }

  let parse xml =
    Some
      { code =
          Util.option_bind
            (Xml.member "Code" xml)
            InstanceFleetStateChangeReasonCode.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.code (fun f ->
               Query.Pair ("Code", InstanceFleetStateChangeReasonCode.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.code (fun f ->
               "code", InstanceFleetStateChangeReasonCode.to_json f)
         ])

  let of_json j =
    { code =
        Util.option_map (Json.lookup j "code") InstanceFleetStateChangeReasonCode.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    }
end

module InstanceFleetTimeline = struct
  type t =
    { creation_date_time : DateTime.t option
    ; ready_date_time : DateTime.t option
    ; end_date_time : DateTime.t option
    }

  let make ?creation_date_time ?ready_date_time ?end_date_time () =
    { creation_date_time; ready_date_time; end_date_time }

  let parse xml =
    Some
      { creation_date_time =
          Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse
      ; ready_date_time = Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse
      ; end_date_time = Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f ->
               Query.Pair ("EndDateTime", DateTime.to_query f))
         ; Util.option_map v.ready_date_time (fun f ->
               Query.Pair ("ReadyDateTime", DateTime.to_query f))
         ; Util.option_map v.creation_date_time (fun f ->
               Query.Pair ("CreationDateTime", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f -> "end_date_time", DateTime.to_json f)
         ; Util.option_map v.ready_date_time (fun f ->
               "ready_date_time", DateTime.to_json f)
         ; Util.option_map v.creation_date_time (fun f ->
               "creation_date_time", DateTime.to_json f)
         ])

  let of_json j =
    { creation_date_time =
        Util.option_map (Json.lookup j "creation_date_time") DateTime.of_json
    ; ready_date_time = Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json
    ; end_date_time = Util.option_map (Json.lookup j "end_date_time") DateTime.of_json
    }
end

module InstanceTypeSpecification = struct
  type t =
    { instance_type : String.t option
    ; weighted_capacity : Integer.t option
    ; bid_price : String.t option
    ; bid_price_as_percentage_of_on_demand_price : Double.t option
    ; configurations : ConfigurationList.t
    ; ebs_block_devices : EbsBlockDeviceList.t
    ; ebs_optimized : Boolean.t option
    }

  let make
      ?instance_type
      ?weighted_capacity
      ?bid_price
      ?bid_price_as_percentage_of_on_demand_price
      ?(configurations = [])
      ?(ebs_block_devices = [])
      ?ebs_optimized
      () =
    { instance_type
    ; weighted_capacity
    ; bid_price
    ; bid_price_as_percentage_of_on_demand_price
    ; configurations
    ; ebs_block_devices
    ; ebs_optimized
    }

  let parse xml =
    Some
      { instance_type = Util.option_bind (Xml.member "InstanceType" xml) String.parse
      ; weighted_capacity =
          Util.option_bind (Xml.member "WeightedCapacity" xml) Integer.parse
      ; bid_price = Util.option_bind (Xml.member "BidPrice" xml) String.parse
      ; bid_price_as_percentage_of_on_demand_price =
          Util.option_bind
            (Xml.member "BidPriceAsPercentageOfOnDemandPrice" xml)
            Double.parse
      ; configurations =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Configurations" xml) ConfigurationList.parse)
      ; ebs_block_devices =
          Util.of_option
            []
            (Util.option_bind (Xml.member "EbsBlockDevices" xml) EbsBlockDeviceList.parse)
      ; ebs_optimized = Util.option_bind (Xml.member "EbsOptimized" xml) Boolean.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.ebs_optimized (fun f ->
               Query.Pair ("EbsOptimized", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("EbsBlockDevices.member", EbsBlockDeviceList.to_query v.ebs_block_devices))
         ; Some
             (Query.Pair
                ("Configurations.member", ConfigurationList.to_query v.configurations))
         ; Util.option_map v.bid_price_as_percentage_of_on_demand_price (fun f ->
               Query.Pair ("BidPriceAsPercentageOfOnDemandPrice", Double.to_query f))
         ; Util.option_map v.bid_price (fun f ->
               Query.Pair ("BidPrice", String.to_query f))
         ; Util.option_map v.weighted_capacity (fun f ->
               Query.Pair ("WeightedCapacity", Integer.to_query f))
         ; Util.option_map v.instance_type (fun f ->
               Query.Pair ("InstanceType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.ebs_optimized (fun f -> "ebs_optimized", Boolean.to_json f)
         ; Some ("ebs_block_devices", EbsBlockDeviceList.to_json v.ebs_block_devices)
         ; Some ("configurations", ConfigurationList.to_json v.configurations)
         ; Util.option_map v.bid_price_as_percentage_of_on_demand_price (fun f ->
               "bid_price_as_percentage_of_on_demand_price", Double.to_json f)
         ; Util.option_map v.bid_price (fun f -> "bid_price", String.to_json f)
         ; Util.option_map v.weighted_capacity (fun f ->
               "weighted_capacity", Integer.to_json f)
         ; Util.option_map v.instance_type (fun f -> "instance_type", String.to_json f)
         ])

  let of_json j =
    { instance_type = Util.option_map (Json.lookup j "instance_type") String.of_json
    ; weighted_capacity =
        Util.option_map (Json.lookup j "weighted_capacity") Integer.of_json
    ; bid_price = Util.option_map (Json.lookup j "bid_price") String.of_json
    ; bid_price_as_percentage_of_on_demand_price =
        Util.option_map
          (Json.lookup j "bid_price_as_percentage_of_on_demand_price")
          Double.of_json
    ; configurations =
        ConfigurationList.of_json (Util.of_option_exn (Json.lookup j "configurations"))
    ; ebs_block_devices =
        EbsBlockDeviceList.of_json
          (Util.of_option_exn (Json.lookup j "ebs_block_devices"))
    ; ebs_optimized = Util.option_map (Json.lookup j "ebs_optimized") Boolean.of_json
    }
end

module FailureDetails = struct
  type t =
    { reason : String.t option
    ; message : String.t option
    ; log_file : String.t option
    }

  let make ?reason ?message ?log_file () = { reason; message; log_file }

  let parse xml =
    Some
      { reason = Util.option_bind (Xml.member "Reason" xml) String.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      ; log_file = Util.option_bind (Xml.member "LogFile" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.log_file (fun f -> Query.Pair ("LogFile", String.to_query f))
         ; Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.reason (fun f -> Query.Pair ("Reason", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.log_file (fun f -> "log_file", String.to_json f)
         ; Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.reason (fun f -> "reason", String.to_json f)
         ])

  let of_json j =
    { reason = Util.option_map (Json.lookup j "reason") String.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    ; log_file = Util.option_map (Json.lookup j "log_file") String.of_json
    }
end

module StepState = struct
  type t =
    | PENDING
    | CANCEL_PENDING
    | RUNNING
    | COMPLETED
    | CANCELLED
    | FAILED
    | INTERRUPTED

  let str_to_t =
    [ "INTERRUPTED", INTERRUPTED
    ; "FAILED", FAILED
    ; "CANCELLED", CANCELLED
    ; "COMPLETED", COMPLETED
    ; "RUNNING", RUNNING
    ; "CANCEL_PENDING", CANCEL_PENDING
    ; "PENDING", PENDING
    ]

  let t_to_str =
    [ INTERRUPTED, "INTERRUPTED"
    ; FAILED, "FAILED"
    ; CANCELLED, "CANCELLED"
    ; COMPLETED, "COMPLETED"
    ; RUNNING, "RUNNING"
    ; CANCEL_PENDING, "CANCEL_PENDING"
    ; PENDING, "PENDING"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module StepStateChangeReason = struct
  type t =
    { code : StepStateChangeReasonCode.t option
    ; message : String.t option
    }

  let make ?code ?message () = { code; message }

  let parse xml =
    Some
      { code = Util.option_bind (Xml.member "Code" xml) StepStateChangeReasonCode.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.code (fun f ->
               Query.Pair ("Code", StepStateChangeReasonCode.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.code (fun f -> "code", StepStateChangeReasonCode.to_json f)
         ])

  let of_json j =
    { code = Util.option_map (Json.lookup j "code") StepStateChangeReasonCode.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    }
end

module StepTimeline = struct
  type t =
    { creation_date_time : DateTime.t option
    ; start_date_time : DateTime.t option
    ; end_date_time : DateTime.t option
    }

  let make ?creation_date_time ?start_date_time ?end_date_time () =
    { creation_date_time; start_date_time; end_date_time }

  let parse xml =
    Some
      { creation_date_time =
          Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse
      ; start_date_time = Util.option_bind (Xml.member "StartDateTime" xml) DateTime.parse
      ; end_date_time = Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f ->
               Query.Pair ("EndDateTime", DateTime.to_query f))
         ; Util.option_map v.start_date_time (fun f ->
               Query.Pair ("StartDateTime", DateTime.to_query f))
         ; Util.option_map v.creation_date_time (fun f ->
               Query.Pair ("CreationDateTime", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f -> "end_date_time", DateTime.to_json f)
         ; Util.option_map v.start_date_time (fun f ->
               "start_date_time", DateTime.to_json f)
         ; Util.option_map v.creation_date_time (fun f ->
               "creation_date_time", DateTime.to_json f)
         ])

  let of_json j =
    { creation_date_time =
        Util.option_map (Json.lookup j "creation_date_time") DateTime.of_json
    ; start_date_time = Util.option_map (Json.lookup j "start_date_time") DateTime.of_json
    ; end_date_time = Util.option_map (Json.lookup j "end_date_time") DateTime.of_json
    }
end

module InstanceResizePolicy = struct
  type t =
    { instances_to_terminate : EC2InstanceIdsList.t
    ; instances_to_protect : EC2InstanceIdsList.t
    ; instance_termination_timeout : Integer.t option
    }

  let make
      ?(instances_to_terminate = [])
      ?(instances_to_protect = [])
      ?instance_termination_timeout
      () =
    { instances_to_terminate; instances_to_protect; instance_termination_timeout }

  let parse xml =
    Some
      { instances_to_terminate =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstancesToTerminate" xml)
               EC2InstanceIdsList.parse)
      ; instances_to_protect =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstancesToProtect" xml)
               EC2InstanceIdsList.parse)
      ; instance_termination_timeout =
          Util.option_bind (Xml.member "InstanceTerminationTimeout" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.instance_termination_timeout (fun f ->
               Query.Pair ("InstanceTerminationTimeout", Integer.to_query f))
         ; Some
             (Query.Pair
                ( "InstancesToProtect.member"
                , EC2InstanceIdsList.to_query v.instances_to_protect ))
         ; Some
             (Query.Pair
                ( "InstancesToTerminate.member"
                , EC2InstanceIdsList.to_query v.instances_to_terminate ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.instance_termination_timeout (fun f ->
               "instance_termination_timeout", Integer.to_json f)
         ; Some ("instances_to_protect", EC2InstanceIdsList.to_json v.instances_to_protect)
         ; Some
             ( "instances_to_terminate"
             , EC2InstanceIdsList.to_json v.instances_to_terminate )
         ])

  let of_json j =
    { instances_to_terminate =
        EC2InstanceIdsList.of_json
          (Util.of_option_exn (Json.lookup j "instances_to_terminate"))
    ; instances_to_protect =
        EC2InstanceIdsList.of_json
          (Util.of_option_exn (Json.lookup j "instances_to_protect"))
    ; instance_termination_timeout =
        Util.option_map (Json.lookup j "instance_termination_timeout") Integer.of_json
    }
end

module AutoScalingPolicyStatus = struct
  type t =
    { state : AutoScalingPolicyState.t option
    ; state_change_reason : AutoScalingPolicyStateChangeReason.t option
    }

  let make ?state ?state_change_reason () = { state; state_change_reason }

  let parse xml =
    Some
      { state = Util.option_bind (Xml.member "State" xml) AutoScalingPolicyState.parse
      ; state_change_reason =
          Util.option_bind
            (Xml.member "StateChangeReason" xml)
            AutoScalingPolicyStateChangeReason.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.state_change_reason (fun f ->
               Query.Pair
                 ("StateChangeReason", AutoScalingPolicyStateChangeReason.to_query f))
         ; Util.option_map v.state (fun f ->
               Query.Pair ("State", AutoScalingPolicyState.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.state_change_reason (fun f ->
               "state_change_reason", AutoScalingPolicyStateChangeReason.to_json f)
         ; Util.option_map v.state (fun f -> "state", AutoScalingPolicyState.to_json f)
         ])

  let of_json j =
    { state = Util.option_map (Json.lookup j "state") AutoScalingPolicyState.of_json
    ; state_change_reason =
        Util.option_map
          (Json.lookup j "state_change_reason")
          AutoScalingPolicyStateChangeReason.of_json
    }
end

module InstanceGroupStateChangeReason = struct
  type t =
    { code : InstanceGroupStateChangeReasonCode.t option
    ; message : String.t option
    }

  let make ?code ?message () = { code; message }

  let parse xml =
    Some
      { code =
          Util.option_bind
            (Xml.member "Code" xml)
            InstanceGroupStateChangeReasonCode.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.code (fun f ->
               Query.Pair ("Code", InstanceGroupStateChangeReasonCode.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.code (fun f ->
               "code", InstanceGroupStateChangeReasonCode.to_json f)
         ])

  let of_json j =
    { code =
        Util.option_map (Json.lookup j "code") InstanceGroupStateChangeReasonCode.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    }
end

module InstanceGroupTimeline = struct
  type t =
    { creation_date_time : DateTime.t option
    ; ready_date_time : DateTime.t option
    ; end_date_time : DateTime.t option
    }

  let make ?creation_date_time ?ready_date_time ?end_date_time () =
    { creation_date_time; ready_date_time; end_date_time }

  let parse xml =
    Some
      { creation_date_time =
          Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse
      ; ready_date_time = Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse
      ; end_date_time = Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f ->
               Query.Pair ("EndDateTime", DateTime.to_query f))
         ; Util.option_map v.ready_date_time (fun f ->
               Query.Pair ("ReadyDateTime", DateTime.to_query f))
         ; Util.option_map v.creation_date_time (fun f ->
               Query.Pair ("CreationDateTime", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f -> "end_date_time", DateTime.to_json f)
         ; Util.option_map v.ready_date_time (fun f ->
               "ready_date_time", DateTime.to_json f)
         ; Util.option_map v.creation_date_time (fun f ->
               "creation_date_time", DateTime.to_json f)
         ])

  let of_json j =
    { creation_date_time =
        Util.option_map (Json.lookup j "creation_date_time") DateTime.of_json
    ; ready_date_time = Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json
    ; end_date_time = Util.option_map (Json.lookup j "end_date_time") DateTime.of_json
    }
end

module EbsVolume = struct
  type t =
    { device : String.t option
    ; volume_id : String.t option
    }

  let make ?device ?volume_id () = { device; volume_id }

  let parse xml =
    Some
      { device = Util.option_bind (Xml.member "Device" xml) String.parse
      ; volume_id = Util.option_bind (Xml.member "VolumeId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.volume_id (fun f ->
               Query.Pair ("VolumeId", String.to_query f))
         ; Util.option_map v.device (fun f -> Query.Pair ("Device", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.volume_id (fun f -> "volume_id", String.to_json f)
         ; Util.option_map v.device (fun f -> "device", String.to_json f)
         ])

  let of_json j =
    { device = Util.option_map (Json.lookup j "device") String.of_json
    ; volume_id = Util.option_map (Json.lookup j "volume_id") String.of_json
    }
end

module InstanceState = struct
  type t =
    | AWAITING_FULFILLMENT
    | PROVISIONING
    | BOOTSTRAPPING
    | RUNNING
    | TERMINATED

  let str_to_t =
    [ "TERMINATED", TERMINATED
    ; "RUNNING", RUNNING
    ; "BOOTSTRAPPING", BOOTSTRAPPING
    ; "PROVISIONING", PROVISIONING
    ; "AWAITING_FULFILLMENT", AWAITING_FULFILLMENT
    ]

  let t_to_str =
    [ TERMINATED, "TERMINATED"
    ; RUNNING, "RUNNING"
    ; BOOTSTRAPPING, "BOOTSTRAPPING"
    ; PROVISIONING, "PROVISIONING"
    ; AWAITING_FULFILLMENT, "AWAITING_FULFILLMENT"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module InstanceStateChangeReason = struct
  type t =
    { code : InstanceStateChangeReasonCode.t option
    ; message : String.t option
    }

  let make ?code ?message () = { code; message }

  let parse xml =
    Some
      { code =
          Util.option_bind (Xml.member "Code" xml) InstanceStateChangeReasonCode.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.code (fun f ->
               Query.Pair ("Code", InstanceStateChangeReasonCode.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.code (fun f ->
               "code", InstanceStateChangeReasonCode.to_json f)
         ])

  let of_json j =
    { code = Util.option_map (Json.lookup j "code") InstanceStateChangeReasonCode.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    }
end

module InstanceTimeline = struct
  type t =
    { creation_date_time : DateTime.t option
    ; ready_date_time : DateTime.t option
    ; end_date_time : DateTime.t option
    }

  let make ?creation_date_time ?ready_date_time ?end_date_time () =
    { creation_date_time; ready_date_time; end_date_time }

  let parse xml =
    Some
      { creation_date_time =
          Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse
      ; ready_date_time = Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse
      ; end_date_time = Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f ->
               Query.Pair ("EndDateTime", DateTime.to_query f))
         ; Util.option_map v.ready_date_time (fun f ->
               Query.Pair ("ReadyDateTime", DateTime.to_query f))
         ; Util.option_map v.creation_date_time (fun f ->
               Query.Pair ("CreationDateTime", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f -> "end_date_time", DateTime.to_json f)
         ; Util.option_map v.ready_date_time (fun f ->
               "ready_date_time", DateTime.to_json f)
         ; Util.option_map v.creation_date_time (fun f ->
               "creation_date_time", DateTime.to_json f)
         ])

  let of_json j =
    { creation_date_time =
        Util.option_map (Json.lookup j "creation_date_time") DateTime.of_json
    ; ready_date_time = Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json
    ; end_date_time = Util.option_map (Json.lookup j "end_date_time") DateTime.of_json
    }
end

module ClusterState = struct
  type t =
    | STARTING
    | BOOTSTRAPPING
    | RUNNING
    | WAITING
    | TERMINATING
    | TERMINATED
    | TERMINATED_WITH_ERRORS

  let str_to_t =
    [ "TERMINATED_WITH_ERRORS", TERMINATED_WITH_ERRORS
    ; "TERMINATED", TERMINATED
    ; "TERMINATING", TERMINATING
    ; "WAITING", WAITING
    ; "RUNNING", RUNNING
    ; "BOOTSTRAPPING", BOOTSTRAPPING
    ; "STARTING", STARTING
    ]

  let t_to_str =
    [ TERMINATED_WITH_ERRORS, "TERMINATED_WITH_ERRORS"
    ; TERMINATED, "TERMINATED"
    ; TERMINATING, "TERMINATING"
    ; WAITING, "WAITING"
    ; RUNNING, "RUNNING"
    ; BOOTSTRAPPING, "BOOTSTRAPPING"
    ; STARTING, "STARTING"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ClusterStateChangeReason = struct
  type t =
    { code : ClusterStateChangeReasonCode.t option
    ; message : String.t option
    }

  let make ?code ?message () = { code; message }

  let parse xml =
    Some
      { code = Util.option_bind (Xml.member "Code" xml) ClusterStateChangeReasonCode.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.code (fun f ->
               Query.Pair ("Code", ClusterStateChangeReasonCode.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.code (fun f ->
               "code", ClusterStateChangeReasonCode.to_json f)
         ])

  let of_json j =
    { code = Util.option_map (Json.lookup j "code") ClusterStateChangeReasonCode.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    }
end

module ClusterTimeline = struct
  type t =
    { creation_date_time : DateTime.t option
    ; ready_date_time : DateTime.t option
    ; end_date_time : DateTime.t option
    }

  let make ?creation_date_time ?ready_date_time ?end_date_time () =
    { creation_date_time; ready_date_time; end_date_time }

  let parse xml =
    Some
      { creation_date_time =
          Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse
      ; ready_date_time = Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse
      ; end_date_time = Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f ->
               Query.Pair ("EndDateTime", DateTime.to_query f))
         ; Util.option_map v.ready_date_time (fun f ->
               Query.Pair ("ReadyDateTime", DateTime.to_query f))
         ; Util.option_map v.creation_date_time (fun f ->
               Query.Pair ("CreationDateTime", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.end_date_time (fun f -> "end_date_time", DateTime.to_json f)
         ; Util.option_map v.ready_date_time (fun f ->
               "ready_date_time", DateTime.to_json f)
         ; Util.option_map v.creation_date_time (fun f ->
               "creation_date_time", DateTime.to_json f)
         ])

  let of_json j =
    { creation_date_time =
        Util.option_map (Json.lookup j "creation_date_time") DateTime.of_json
    ; ready_date_time = Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json
    ; end_date_time = Util.option_map (Json.lookup j "end_date_time") DateTime.of_json
    }
end

module BootstrapActionDetail = struct
  type t = { bootstrap_action_config : BootstrapActionConfig.t option }

  let make ?bootstrap_action_config () = { bootstrap_action_config }

  let parse xml =
    Some
      { bootstrap_action_config =
          Util.option_bind
            (Xml.member "BootstrapActionConfig" xml)
            BootstrapActionConfig.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.bootstrap_action_config (fun f ->
               Query.Pair ("BootstrapActionConfig", BootstrapActionConfig.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.bootstrap_action_config (fun f ->
               "bootstrap_action_config", BootstrapActionConfig.to_json f)
         ])

  let of_json j =
    { bootstrap_action_config =
        Util.option_map
          (Json.lookup j "bootstrap_action_config")
          BootstrapActionConfig.of_json
    }
end

module JobFlowExecutionState = struct
  type t =
    | STARTING
    | BOOTSTRAPPING
    | RUNNING
    | WAITING
    | SHUTTING_DOWN
    | TERMINATED
    | COMPLETED
    | FAILED

  let str_to_t =
    [ "FAILED", FAILED
    ; "COMPLETED", COMPLETED
    ; "TERMINATED", TERMINATED
    ; "SHUTTING_DOWN", SHUTTING_DOWN
    ; "WAITING", WAITING
    ; "RUNNING", RUNNING
    ; "BOOTSTRAPPING", BOOTSTRAPPING
    ; "STARTING", STARTING
    ]

  let t_to_str =
    [ FAILED, "FAILED"
    ; COMPLETED, "COMPLETED"
    ; TERMINATED, "TERMINATED"
    ; SHUTTING_DOWN, "SHUTTING_DOWN"
    ; WAITING, "WAITING"
    ; RUNNING, "RUNNING"
    ; BOOTSTRAPPING, "BOOTSTRAPPING"
    ; STARTING, "STARTING"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module InstanceGroupDetailList = struct
  type t = InstanceGroupDetail.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceGroupDetail.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceGroupDetail.to_query v

  let to_json v = `List (List.map InstanceGroupDetail.to_json v)

  let of_json j = Json.to_list InstanceGroupDetail.of_json j
end

module PlacementType = struct
  type t =
    { availability_zone : String.t option
    ; availability_zones : XmlStringMaxLen256List.t
    }

  let make ?availability_zone ?(availability_zones = []) () =
    { availability_zone; availability_zones }

  let parse xml =
    Some
      { availability_zone =
          Util.option_bind (Xml.member "AvailabilityZone" xml) String.parse
      ; availability_zones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AvailabilityZones" xml)
               XmlStringMaxLen256List.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "AvailabilityZones.member"
                , XmlStringMaxLen256List.to_query v.availability_zones ))
         ; Util.option_map v.availability_zone (fun f ->
               Query.Pair ("AvailabilityZone", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("availability_zones", XmlStringMaxLen256List.to_json v.availability_zones)
         ; Util.option_map v.availability_zone (fun f ->
               "availability_zone", String.to_json f)
         ])

  let of_json j =
    { availability_zone =
        Util.option_map (Json.lookup j "availability_zone") String.of_json
    ; availability_zones =
        XmlStringMaxLen256List.of_json
          (Util.of_option_exn (Json.lookup j "availability_zones"))
    }
end

module StepDetail = struct
  type t =
    { step_config : StepConfig.t
    ; execution_status_detail : StepExecutionStatusDetail.t
    }

  let make ~step_config ~execution_status_detail () =
    { step_config; execution_status_detail }

  let parse xml =
    Some
      { step_config =
          Xml.required
            "StepConfig"
            (Util.option_bind (Xml.member "StepConfig" xml) StepConfig.parse)
      ; execution_status_detail =
          Xml.required
            "ExecutionStatusDetail"
            (Util.option_bind
               (Xml.member "ExecutionStatusDetail" xml)
               StepExecutionStatusDetail.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "ExecutionStatusDetail"
                , StepExecutionStatusDetail.to_query v.execution_status_detail ))
         ; Some (Query.Pair ("StepConfig", StepConfig.to_query v.step_config))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "execution_status_detail"
             , StepExecutionStatusDetail.to_json v.execution_status_detail )
         ; Some ("step_config", StepConfig.to_json v.step_config)
         ])

  let of_json j =
    { step_config = StepConfig.of_json (Util.of_option_exn (Json.lookup j "step_config"))
    ; execution_status_detail =
        StepExecutionStatusDetail.of_json
          (Util.of_option_exn (Json.lookup j "execution_status_detail"))
    }
end

module Application = struct
  type t =
    { name : String.t option
    ; version : String.t option
    ; args : StringList.t
    ; additional_info : StringMap.t option
    }

  let make ?name ?version ?(args = []) ?additional_info () =
    { name; version; args; additional_info }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; version = Util.option_bind (Xml.member "Version" xml) String.parse
      ; args =
          Util.of_option [] (Util.option_bind (Xml.member "Args" xml) StringList.parse)
      ; additional_info =
          Util.option_bind (Xml.member "AdditionalInfo" xml) StringMap.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.additional_info (fun f ->
               Query.Pair ("AdditionalInfo", StringMap.to_query f))
         ; Some (Query.Pair ("Args.member", StringList.to_query v.args))
         ; Util.option_map v.version (fun f -> Query.Pair ("Version", String.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.additional_info (fun f ->
               "additional_info", StringMap.to_json f)
         ; Some ("args", StringList.to_json v.args)
         ; Util.option_map v.version (fun f -> "version", String.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; version = Util.option_map (Json.lookup j "version") String.of_json
    ; args = StringList.of_json (Util.of_option_exn (Json.lookup j "args"))
    ; additional_info =
        Util.option_map (Json.lookup j "additional_info") StringMap.of_json
    }
end

module Tag = struct
  type t =
    { key : String.t option
    ; value : String.t option
    }

  let make ?key ?value () = { key; value }

  let parse xml =
    Some
      { key = Util.option_bind (Xml.member "Key" xml) String.parse
      ; value = Util.option_bind (Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> Query.Pair ("Value", String.to_query f))
         ; Util.option_map v.key (fun f -> Query.Pair ("Key", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.value (fun f -> "value", String.to_json f)
         ; Util.option_map v.key (fun f -> "key", String.to_json f)
         ])

  let of_json j =
    { key = Util.option_map (Json.lookup j "key") String.of_json
    ; value = Util.option_map (Json.lookup j "value") String.of_json
    }
end

module InstanceFleetConfig = struct
  type t =
    { name : String.t option
    ; instance_fleet_type : InstanceFleetType.t
    ; target_on_demand_capacity : Integer.t option
    ; target_spot_capacity : Integer.t option
    ; instance_type_configs : InstanceTypeConfigList.t
    ; launch_specifications : InstanceFleetProvisioningSpecifications.t option
    }

  let make
      ?name
      ~instance_fleet_type
      ?target_on_demand_capacity
      ?target_spot_capacity
      ?(instance_type_configs = [])
      ?launch_specifications
      () =
    { name
    ; instance_fleet_type
    ; target_on_demand_capacity
    ; target_spot_capacity
    ; instance_type_configs
    ; launch_specifications
    }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; instance_fleet_type =
          Xml.required
            "InstanceFleetType"
            (Util.option_bind
               (Xml.member "InstanceFleetType" xml)
               InstanceFleetType.parse)
      ; target_on_demand_capacity =
          Util.option_bind (Xml.member "TargetOnDemandCapacity" xml) Integer.parse
      ; target_spot_capacity =
          Util.option_bind (Xml.member "TargetSpotCapacity" xml) Integer.parse
      ; instance_type_configs =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstanceTypeConfigs" xml)
               InstanceTypeConfigList.parse)
      ; launch_specifications =
          Util.option_bind
            (Xml.member "LaunchSpecifications" xml)
            InstanceFleetProvisioningSpecifications.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.launch_specifications (fun f ->
               Query.Pair
                 ( "LaunchSpecifications"
                 , InstanceFleetProvisioningSpecifications.to_query f ))
         ; Some
             (Query.Pair
                ( "InstanceTypeConfigs.member"
                , InstanceTypeConfigList.to_query v.instance_type_configs ))
         ; Util.option_map v.target_spot_capacity (fun f ->
               Query.Pair ("TargetSpotCapacity", Integer.to_query f))
         ; Util.option_map v.target_on_demand_capacity (fun f ->
               Query.Pair ("TargetOnDemandCapacity", Integer.to_query f))
         ; Some
             (Query.Pair
                ("InstanceFleetType", InstanceFleetType.to_query v.instance_fleet_type))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.launch_specifications (fun f ->
               "launch_specifications", InstanceFleetProvisioningSpecifications.to_json f)
         ; Some
             ( "instance_type_configs"
             , InstanceTypeConfigList.to_json v.instance_type_configs )
         ; Util.option_map v.target_spot_capacity (fun f ->
               "target_spot_capacity", Integer.to_json f)
         ; Util.option_map v.target_on_demand_capacity (fun f ->
               "target_on_demand_capacity", Integer.to_json f)
         ; Some ("instance_fleet_type", InstanceFleetType.to_json v.instance_fleet_type)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; instance_fleet_type =
        InstanceFleetType.of_json
          (Util.of_option_exn (Json.lookup j "instance_fleet_type"))
    ; target_on_demand_capacity =
        Util.option_map (Json.lookup j "target_on_demand_capacity") Integer.of_json
    ; target_spot_capacity =
        Util.option_map (Json.lookup j "target_spot_capacity") Integer.of_json
    ; instance_type_configs =
        InstanceTypeConfigList.of_json
          (Util.of_option_exn (Json.lookup j "instance_type_configs"))
    ; launch_specifications =
        Util.option_map
          (Json.lookup j "launch_specifications")
          InstanceFleetProvisioningSpecifications.of_json
    }
end

module InstanceGroupConfig = struct
  type t =
    { name : String.t option
    ; market : MarketType.t option
    ; instance_role : InstanceRoleType.t
    ; bid_price : String.t option
    ; instance_type : String.t
    ; instance_count : Integer.t
    ; configurations : ConfigurationList.t
    ; ebs_configuration : EbsConfiguration.t option
    ; auto_scaling_policy : AutoScalingPolicy.t option
    }

  let make
      ?name
      ?market
      ~instance_role
      ?bid_price
      ~instance_type
      ~instance_count
      ?(configurations = [])
      ?ebs_configuration
      ?auto_scaling_policy
      () =
    { name
    ; market
    ; instance_role
    ; bid_price
    ; instance_type
    ; instance_count
    ; configurations
    ; ebs_configuration
    ; auto_scaling_policy
    }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; market = Util.option_bind (Xml.member "Market" xml) MarketType.parse
      ; instance_role =
          Xml.required
            "InstanceRole"
            (Util.option_bind (Xml.member "InstanceRole" xml) InstanceRoleType.parse)
      ; bid_price = Util.option_bind (Xml.member "BidPrice" xml) String.parse
      ; instance_type =
          Xml.required
            "InstanceType"
            (Util.option_bind (Xml.member "InstanceType" xml) String.parse)
      ; instance_count =
          Xml.required
            "InstanceCount"
            (Util.option_bind (Xml.member "InstanceCount" xml) Integer.parse)
      ; configurations =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Configurations" xml) ConfigurationList.parse)
      ; ebs_configuration =
          Util.option_bind (Xml.member "EbsConfiguration" xml) EbsConfiguration.parse
      ; auto_scaling_policy =
          Util.option_bind (Xml.member "AutoScalingPolicy" xml) AutoScalingPolicy.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.auto_scaling_policy (fun f ->
               Query.Pair ("AutoScalingPolicy", AutoScalingPolicy.to_query f))
         ; Util.option_map v.ebs_configuration (fun f ->
               Query.Pair ("EbsConfiguration", EbsConfiguration.to_query f))
         ; Some
             (Query.Pair
                ("Configurations.member", ConfigurationList.to_query v.configurations))
         ; Some (Query.Pair ("InstanceCount", Integer.to_query v.instance_count))
         ; Some (Query.Pair ("InstanceType", String.to_query v.instance_type))
         ; Util.option_map v.bid_price (fun f ->
               Query.Pair ("BidPrice", String.to_query f))
         ; Some (Query.Pair ("InstanceRole", InstanceRoleType.to_query v.instance_role))
         ; Util.option_map v.market (fun f ->
               Query.Pair ("Market", MarketType.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.auto_scaling_policy (fun f ->
               "auto_scaling_policy", AutoScalingPolicy.to_json f)
         ; Util.option_map v.ebs_configuration (fun f ->
               "ebs_configuration", EbsConfiguration.to_json f)
         ; Some ("configurations", ConfigurationList.to_json v.configurations)
         ; Some ("instance_count", Integer.to_json v.instance_count)
         ; Some ("instance_type", String.to_json v.instance_type)
         ; Util.option_map v.bid_price (fun f -> "bid_price", String.to_json f)
         ; Some ("instance_role", InstanceRoleType.to_json v.instance_role)
         ; Util.option_map v.market (fun f -> "market", MarketType.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; market = Util.option_map (Json.lookup j "market") MarketType.of_json
    ; instance_role =
        InstanceRoleType.of_json (Util.of_option_exn (Json.lookup j "instance_role"))
    ; bid_price = Util.option_map (Json.lookup j "bid_price") String.of_json
    ; instance_type = String.of_json (Util.of_option_exn (Json.lookup j "instance_type"))
    ; instance_count =
        Integer.of_json (Util.of_option_exn (Json.lookup j "instance_count"))
    ; configurations =
        ConfigurationList.of_json (Util.of_option_exn (Json.lookup j "configurations"))
    ; ebs_configuration =
        Util.option_map (Json.lookup j "ebs_configuration") EbsConfiguration.of_json
    ; auto_scaling_policy =
        Util.option_map (Json.lookup j "auto_scaling_policy") AutoScalingPolicy.of_json
    }
end

module InstanceFleetStatus = struct
  type t =
    { state : InstanceFleetState.t option
    ; state_change_reason : InstanceFleetStateChangeReason.t option
    ; timeline : InstanceFleetTimeline.t option
    }

  let make ?state ?state_change_reason ?timeline () =
    { state; state_change_reason; timeline }

  let parse xml =
    Some
      { state = Util.option_bind (Xml.member "State" xml) InstanceFleetState.parse
      ; state_change_reason =
          Util.option_bind
            (Xml.member "StateChangeReason" xml)
            InstanceFleetStateChangeReason.parse
      ; timeline =
          Util.option_bind (Xml.member "Timeline" xml) InstanceFleetTimeline.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f ->
               Query.Pair ("Timeline", InstanceFleetTimeline.to_query f))
         ; Util.option_map v.state_change_reason (fun f ->
               Query.Pair ("StateChangeReason", InstanceFleetStateChangeReason.to_query f))
         ; Util.option_map v.state (fun f ->
               Query.Pair ("State", InstanceFleetState.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f ->
               "timeline", InstanceFleetTimeline.to_json f)
         ; Util.option_map v.state_change_reason (fun f ->
               "state_change_reason", InstanceFleetStateChangeReason.to_json f)
         ; Util.option_map v.state (fun f -> "state", InstanceFleetState.to_json f)
         ])

  let of_json j =
    { state = Util.option_map (Json.lookup j "state") InstanceFleetState.of_json
    ; state_change_reason =
        Util.option_map
          (Json.lookup j "state_change_reason")
          InstanceFleetStateChangeReason.of_json
    ; timeline = Util.option_map (Json.lookup j "timeline") InstanceFleetTimeline.of_json
    }
end

module InstanceTypeSpecificationList = struct
  type t = InstanceTypeSpecification.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceTypeSpecification.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceTypeSpecification.to_query v

  let to_json v = `List (List.map InstanceTypeSpecification.to_json v)

  let of_json j = Json.to_list InstanceTypeSpecification.of_json j
end

module HadoopStepConfig = struct
  type t =
    { jar : String.t option
    ; properties : StringMap.t option
    ; main_class : String.t option
    ; args : StringList.t
    }

  let make ?jar ?properties ?main_class ?(args = []) () =
    { jar; properties; main_class; args }

  let parse xml =
    Some
      { jar = Util.option_bind (Xml.member "Jar" xml) String.parse
      ; properties = Util.option_bind (Xml.member "Properties" xml) StringMap.parse
      ; main_class = Util.option_bind (Xml.member "MainClass" xml) String.parse
      ; args =
          Util.of_option [] (Util.option_bind (Xml.member "Args" xml) StringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Args.member", StringList.to_query v.args))
         ; Util.option_map v.main_class (fun f ->
               Query.Pair ("MainClass", String.to_query f))
         ; Util.option_map v.properties (fun f ->
               Query.Pair ("Properties", StringMap.to_query f))
         ; Util.option_map v.jar (fun f -> Query.Pair ("Jar", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("args", StringList.to_json v.args)
         ; Util.option_map v.main_class (fun f -> "main_class", String.to_json f)
         ; Util.option_map v.properties (fun f -> "properties", StringMap.to_json f)
         ; Util.option_map v.jar (fun f -> "jar", String.to_json f)
         ])

  let of_json j =
    { jar = Util.option_map (Json.lookup j "jar") String.of_json
    ; properties = Util.option_map (Json.lookup j "properties") StringMap.of_json
    ; main_class = Util.option_map (Json.lookup j "main_class") String.of_json
    ; args = StringList.of_json (Util.of_option_exn (Json.lookup j "args"))
    }
end

module StepStatus = struct
  type t =
    { state : StepState.t option
    ; state_change_reason : StepStateChangeReason.t option
    ; failure_details : FailureDetails.t option
    ; timeline : StepTimeline.t option
    }

  let make ?state ?state_change_reason ?failure_details ?timeline () =
    { state; state_change_reason; failure_details; timeline }

  let parse xml =
    Some
      { state = Util.option_bind (Xml.member "State" xml) StepState.parse
      ; state_change_reason =
          Util.option_bind
            (Xml.member "StateChangeReason" xml)
            StepStateChangeReason.parse
      ; failure_details =
          Util.option_bind (Xml.member "FailureDetails" xml) FailureDetails.parse
      ; timeline = Util.option_bind (Xml.member "Timeline" xml) StepTimeline.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f ->
               Query.Pair ("Timeline", StepTimeline.to_query f))
         ; Util.option_map v.failure_details (fun f ->
               Query.Pair ("FailureDetails", FailureDetails.to_query f))
         ; Util.option_map v.state_change_reason (fun f ->
               Query.Pair ("StateChangeReason", StepStateChangeReason.to_query f))
         ; Util.option_map v.state (fun f -> Query.Pair ("State", StepState.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f -> "timeline", StepTimeline.to_json f)
         ; Util.option_map v.failure_details (fun f ->
               "failure_details", FailureDetails.to_json f)
         ; Util.option_map v.state_change_reason (fun f ->
               "state_change_reason", StepStateChangeReason.to_json f)
         ; Util.option_map v.state (fun f -> "state", StepState.to_json f)
         ])

  let of_json j =
    { state = Util.option_map (Json.lookup j "state") StepState.of_json
    ; state_change_reason =
        Util.option_map
          (Json.lookup j "state_change_reason")
          StepStateChangeReason.of_json
    ; failure_details =
        Util.option_map (Json.lookup j "failure_details") FailureDetails.of_json
    ; timeline = Util.option_map (Json.lookup j "timeline") StepTimeline.of_json
    }
end

module EC2InstanceIdsToTerminateList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module ShrinkPolicy = struct
  type t =
    { decommission_timeout : Integer.t option
    ; instance_resize_policy : InstanceResizePolicy.t option
    }

  let make ?decommission_timeout ?instance_resize_policy () =
    { decommission_timeout; instance_resize_policy }

  let parse xml =
    Some
      { decommission_timeout =
          Util.option_bind (Xml.member "DecommissionTimeout" xml) Integer.parse
      ; instance_resize_policy =
          Util.option_bind
            (Xml.member "InstanceResizePolicy" xml)
            InstanceResizePolicy.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.instance_resize_policy (fun f ->
               Query.Pair ("InstanceResizePolicy", InstanceResizePolicy.to_query f))
         ; Util.option_map v.decommission_timeout (fun f ->
               Query.Pair ("DecommissionTimeout", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.instance_resize_policy (fun f ->
               "instance_resize_policy", InstanceResizePolicy.to_json f)
         ; Util.option_map v.decommission_timeout (fun f ->
               "decommission_timeout", Integer.to_json f)
         ])

  let of_json j =
    { decommission_timeout =
        Util.option_map (Json.lookup j "decommission_timeout") Integer.of_json
    ; instance_resize_policy =
        Util.option_map
          (Json.lookup j "instance_resize_policy")
          InstanceResizePolicy.of_json
    }
end

module AutoScalingPolicyDescription = struct
  type t =
    { status : AutoScalingPolicyStatus.t option
    ; constraints : ScalingConstraints.t option
    ; rules : ScalingRuleList.t
    }

  let make ?status ?constraints ?(rules = []) () = { status; constraints; rules }

  let parse xml =
    Some
      { status = Util.option_bind (Xml.member "Status" xml) AutoScalingPolicyStatus.parse
      ; constraints =
          Util.option_bind (Xml.member "Constraints" xml) ScalingConstraints.parse
      ; rules =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Rules" xml) ScalingRuleList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Rules.member", ScalingRuleList.to_query v.rules))
         ; Util.option_map v.constraints (fun f ->
               Query.Pair ("Constraints", ScalingConstraints.to_query f))
         ; Util.option_map v.status (fun f ->
               Query.Pair ("Status", AutoScalingPolicyStatus.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("rules", ScalingRuleList.to_json v.rules)
         ; Util.option_map v.constraints (fun f ->
               "constraints", ScalingConstraints.to_json f)
         ; Util.option_map v.status (fun f -> "status", AutoScalingPolicyStatus.to_json f)
         ])

  let of_json j =
    { status = Util.option_map (Json.lookup j "status") AutoScalingPolicyStatus.of_json
    ; constraints =
        Util.option_map (Json.lookup j "constraints") ScalingConstraints.of_json
    ; rules = ScalingRuleList.of_json (Util.of_option_exn (Json.lookup j "rules"))
    }
end

module InstanceGroupStatus = struct
  type t =
    { state : InstanceGroupState.t option
    ; state_change_reason : InstanceGroupStateChangeReason.t option
    ; timeline : InstanceGroupTimeline.t option
    }

  let make ?state ?state_change_reason ?timeline () =
    { state; state_change_reason; timeline }

  let parse xml =
    Some
      { state = Util.option_bind (Xml.member "State" xml) InstanceGroupState.parse
      ; state_change_reason =
          Util.option_bind
            (Xml.member "StateChangeReason" xml)
            InstanceGroupStateChangeReason.parse
      ; timeline =
          Util.option_bind (Xml.member "Timeline" xml) InstanceGroupTimeline.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f ->
               Query.Pair ("Timeline", InstanceGroupTimeline.to_query f))
         ; Util.option_map v.state_change_reason (fun f ->
               Query.Pair ("StateChangeReason", InstanceGroupStateChangeReason.to_query f))
         ; Util.option_map v.state (fun f ->
               Query.Pair ("State", InstanceGroupState.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f ->
               "timeline", InstanceGroupTimeline.to_json f)
         ; Util.option_map v.state_change_reason (fun f ->
               "state_change_reason", InstanceGroupStateChangeReason.to_json f)
         ; Util.option_map v.state (fun f -> "state", InstanceGroupState.to_json f)
         ])

  let of_json j =
    { state = Util.option_map (Json.lookup j "state") InstanceGroupState.of_json
    ; state_change_reason =
        Util.option_map
          (Json.lookup j "state_change_reason")
          InstanceGroupStateChangeReason.of_json
    ; timeline = Util.option_map (Json.lookup j "timeline") InstanceGroupTimeline.of_json
    }
end

module InstanceGroupType = struct
  type t =
    | MASTER
    | CORE
    | TASK

  let str_to_t = [ "TASK", TASK; "CORE", CORE; "MASTER", MASTER ]

  let t_to_str = [ TASK, "TASK"; CORE, "CORE"; MASTER, "MASTER" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module EbsVolumeList = struct
  type t = EbsVolume.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map EbsVolume.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list EbsVolume.to_query v

  let to_json v = `List (List.map EbsVolume.to_json v)

  let of_json j = Json.to_list EbsVolume.of_json j
end

module InstanceStatus = struct
  type t =
    { state : InstanceState.t option
    ; state_change_reason : InstanceStateChangeReason.t option
    ; timeline : InstanceTimeline.t option
    }

  let make ?state ?state_change_reason ?timeline () =
    { state; state_change_reason; timeline }

  let parse xml =
    Some
      { state = Util.option_bind (Xml.member "State" xml) InstanceState.parse
      ; state_change_reason =
          Util.option_bind
            (Xml.member "StateChangeReason" xml)
            InstanceStateChangeReason.parse
      ; timeline = Util.option_bind (Xml.member "Timeline" xml) InstanceTimeline.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f ->
               Query.Pair ("Timeline", InstanceTimeline.to_query f))
         ; Util.option_map v.state_change_reason (fun f ->
               Query.Pair ("StateChangeReason", InstanceStateChangeReason.to_query f))
         ; Util.option_map v.state (fun f ->
               Query.Pair ("State", InstanceState.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f -> "timeline", InstanceTimeline.to_json f)
         ; Util.option_map v.state_change_reason (fun f ->
               "state_change_reason", InstanceStateChangeReason.to_json f)
         ; Util.option_map v.state (fun f -> "state", InstanceState.to_json f)
         ])

  let of_json j =
    { state = Util.option_map (Json.lookup j "state") InstanceState.of_json
    ; state_change_reason =
        Util.option_map
          (Json.lookup j "state_change_reason")
          InstanceStateChangeReason.of_json
    ; timeline = Util.option_map (Json.lookup j "timeline") InstanceTimeline.of_json
    }
end

module ClusterStatus = struct
  type t =
    { state : ClusterState.t option
    ; state_change_reason : ClusterStateChangeReason.t option
    ; timeline : ClusterTimeline.t option
    }

  let make ?state ?state_change_reason ?timeline () =
    { state; state_change_reason; timeline }

  let parse xml =
    Some
      { state = Util.option_bind (Xml.member "State" xml) ClusterState.parse
      ; state_change_reason =
          Util.option_bind
            (Xml.member "StateChangeReason" xml)
            ClusterStateChangeReason.parse
      ; timeline = Util.option_bind (Xml.member "Timeline" xml) ClusterTimeline.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f ->
               Query.Pair ("Timeline", ClusterTimeline.to_query f))
         ; Util.option_map v.state_change_reason (fun f ->
               Query.Pair ("StateChangeReason", ClusterStateChangeReason.to_query f))
         ; Util.option_map v.state (fun f ->
               Query.Pair ("State", ClusterState.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.timeline (fun f -> "timeline", ClusterTimeline.to_json f)
         ; Util.option_map v.state_change_reason (fun f ->
               "state_change_reason", ClusterStateChangeReason.to_json f)
         ; Util.option_map v.state (fun f -> "state", ClusterState.to_json f)
         ])

  let of_json j =
    { state = Util.option_map (Json.lookup j "state") ClusterState.of_json
    ; state_change_reason =
        Util.option_map
          (Json.lookup j "state_change_reason")
          ClusterStateChangeReason.of_json
    ; timeline = Util.option_map (Json.lookup j "timeline") ClusterTimeline.of_json
    }
end

module BootstrapActionDetailList = struct
  type t = BootstrapActionDetail.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map BootstrapActionDetail.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list BootstrapActionDetail.to_query v

  let to_json v = `List (List.map BootstrapActionDetail.to_json v)

  let of_json j = Json.to_list BootstrapActionDetail.of_json j
end

module JobFlowExecutionStatusDetail = struct
  type t =
    { state : JobFlowExecutionState.t
    ; creation_date_time : DateTime.t
    ; start_date_time : DateTime.t option
    ; ready_date_time : DateTime.t option
    ; end_date_time : DateTime.t option
    ; last_state_change_reason : String.t option
    }

  let make
      ~state
      ~creation_date_time
      ?start_date_time
      ?ready_date_time
      ?end_date_time
      ?last_state_change_reason
      () =
    { state
    ; creation_date_time
    ; start_date_time
    ; ready_date_time
    ; end_date_time
    ; last_state_change_reason
    }

  let parse xml =
    Some
      { state =
          Xml.required
            "State"
            (Util.option_bind (Xml.member "State" xml) JobFlowExecutionState.parse)
      ; creation_date_time =
          Xml.required
            "CreationDateTime"
            (Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse)
      ; start_date_time = Util.option_bind (Xml.member "StartDateTime" xml) DateTime.parse
      ; ready_date_time = Util.option_bind (Xml.member "ReadyDateTime" xml) DateTime.parse
      ; end_date_time = Util.option_bind (Xml.member "EndDateTime" xml) DateTime.parse
      ; last_state_change_reason =
          Util.option_bind (Xml.member "LastStateChangeReason" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.last_state_change_reason (fun f ->
               Query.Pair ("LastStateChangeReason", String.to_query f))
         ; Util.option_map v.end_date_time (fun f ->
               Query.Pair ("EndDateTime", DateTime.to_query f))
         ; Util.option_map v.ready_date_time (fun f ->
               Query.Pair ("ReadyDateTime", DateTime.to_query f))
         ; Util.option_map v.start_date_time (fun f ->
               Query.Pair ("StartDateTime", DateTime.to_query f))
         ; Some (Query.Pair ("CreationDateTime", DateTime.to_query v.creation_date_time))
         ; Some (Query.Pair ("State", JobFlowExecutionState.to_query v.state))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.last_state_change_reason (fun f ->
               "last_state_change_reason", String.to_json f)
         ; Util.option_map v.end_date_time (fun f -> "end_date_time", DateTime.to_json f)
         ; Util.option_map v.ready_date_time (fun f ->
               "ready_date_time", DateTime.to_json f)
         ; Util.option_map v.start_date_time (fun f ->
               "start_date_time", DateTime.to_json f)
         ; Some ("creation_date_time", DateTime.to_json v.creation_date_time)
         ; Some ("state", JobFlowExecutionState.to_json v.state)
         ])

  let of_json j =
    { state = JobFlowExecutionState.of_json (Util.of_option_exn (Json.lookup j "state"))
    ; creation_date_time =
        DateTime.of_json (Util.of_option_exn (Json.lookup j "creation_date_time"))
    ; start_date_time = Util.option_map (Json.lookup j "start_date_time") DateTime.of_json
    ; ready_date_time = Util.option_map (Json.lookup j "ready_date_time") DateTime.of_json
    ; end_date_time = Util.option_map (Json.lookup j "end_date_time") DateTime.of_json
    ; last_state_change_reason =
        Util.option_map (Json.lookup j "last_state_change_reason") String.of_json
    }
end

module JobFlowInstancesDetail = struct
  type t =
    { master_instance_type : String.t
    ; master_public_dns_name : String.t option
    ; master_instance_id : String.t option
    ; slave_instance_type : String.t
    ; instance_count : Integer.t
    ; instance_groups : InstanceGroupDetailList.t
    ; normalized_instance_hours : Integer.t option
    ; ec2_key_name : String.t option
    ; ec2_subnet_id : String.t option
    ; placement : PlacementType.t option
    ; keep_job_flow_alive_when_no_steps : Boolean.t option
    ; termination_protected : Boolean.t option
    ; hadoop_version : String.t option
    }

  let make
      ~master_instance_type
      ?master_public_dns_name
      ?master_instance_id
      ~slave_instance_type
      ~instance_count
      ?(instance_groups = [])
      ?normalized_instance_hours
      ?ec2_key_name
      ?ec2_subnet_id
      ?placement
      ?keep_job_flow_alive_when_no_steps
      ?termination_protected
      ?hadoop_version
      () =
    { master_instance_type
    ; master_public_dns_name
    ; master_instance_id
    ; slave_instance_type
    ; instance_count
    ; instance_groups
    ; normalized_instance_hours
    ; ec2_key_name
    ; ec2_subnet_id
    ; placement
    ; keep_job_flow_alive_when_no_steps
    ; termination_protected
    ; hadoop_version
    }

  let parse xml =
    Some
      { master_instance_type =
          Xml.required
            "MasterInstanceType"
            (Util.option_bind (Xml.member "MasterInstanceType" xml) String.parse)
      ; master_public_dns_name =
          Util.option_bind (Xml.member "MasterPublicDnsName" xml) String.parse
      ; master_instance_id =
          Util.option_bind (Xml.member "MasterInstanceId" xml) String.parse
      ; slave_instance_type =
          Xml.required
            "SlaveInstanceType"
            (Util.option_bind (Xml.member "SlaveInstanceType" xml) String.parse)
      ; instance_count =
          Xml.required
            "InstanceCount"
            (Util.option_bind (Xml.member "InstanceCount" xml) Integer.parse)
      ; instance_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstanceGroups" xml)
               InstanceGroupDetailList.parse)
      ; normalized_instance_hours =
          Util.option_bind (Xml.member "NormalizedInstanceHours" xml) Integer.parse
      ; ec2_key_name = Util.option_bind (Xml.member "Ec2KeyName" xml) String.parse
      ; ec2_subnet_id = Util.option_bind (Xml.member "Ec2SubnetId" xml) String.parse
      ; placement = Util.option_bind (Xml.member "Placement" xml) PlacementType.parse
      ; keep_job_flow_alive_when_no_steps =
          Util.option_bind (Xml.member "KeepJobFlowAliveWhenNoSteps" xml) Boolean.parse
      ; termination_protected =
          Util.option_bind (Xml.member "TerminationProtected" xml) Boolean.parse
      ; hadoop_version = Util.option_bind (Xml.member "HadoopVersion" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.hadoop_version (fun f ->
               Query.Pair ("HadoopVersion", String.to_query f))
         ; Util.option_map v.termination_protected (fun f ->
               Query.Pair ("TerminationProtected", Boolean.to_query f))
         ; Util.option_map v.keep_job_flow_alive_when_no_steps (fun f ->
               Query.Pair ("KeepJobFlowAliveWhenNoSteps", Boolean.to_query f))
         ; Util.option_map v.placement (fun f ->
               Query.Pair ("Placement", PlacementType.to_query f))
         ; Util.option_map v.ec2_subnet_id (fun f ->
               Query.Pair ("Ec2SubnetId", String.to_query f))
         ; Util.option_map v.ec2_key_name (fun f ->
               Query.Pair ("Ec2KeyName", String.to_query f))
         ; Util.option_map v.normalized_instance_hours (fun f ->
               Query.Pair ("NormalizedInstanceHours", Integer.to_query f))
         ; Some
             (Query.Pair
                ( "InstanceGroups.member"
                , InstanceGroupDetailList.to_query v.instance_groups ))
         ; Some (Query.Pair ("InstanceCount", Integer.to_query v.instance_count))
         ; Some (Query.Pair ("SlaveInstanceType", String.to_query v.slave_instance_type))
         ; Util.option_map v.master_instance_id (fun f ->
               Query.Pair ("MasterInstanceId", String.to_query f))
         ; Util.option_map v.master_public_dns_name (fun f ->
               Query.Pair ("MasterPublicDnsName", String.to_query f))
         ; Some
             (Query.Pair ("MasterInstanceType", String.to_query v.master_instance_type))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.hadoop_version (fun f -> "hadoop_version", String.to_json f)
         ; Util.option_map v.termination_protected (fun f ->
               "termination_protected", Boolean.to_json f)
         ; Util.option_map v.keep_job_flow_alive_when_no_steps (fun f ->
               "keep_job_flow_alive_when_no_steps", Boolean.to_json f)
         ; Util.option_map v.placement (fun f -> "placement", PlacementType.to_json f)
         ; Util.option_map v.ec2_subnet_id (fun f -> "ec2_subnet_id", String.to_json f)
         ; Util.option_map v.ec2_key_name (fun f -> "ec2_key_name", String.to_json f)
         ; Util.option_map v.normalized_instance_hours (fun f ->
               "normalized_instance_hours", Integer.to_json f)
         ; Some ("instance_groups", InstanceGroupDetailList.to_json v.instance_groups)
         ; Some ("instance_count", Integer.to_json v.instance_count)
         ; Some ("slave_instance_type", String.to_json v.slave_instance_type)
         ; Util.option_map v.master_instance_id (fun f ->
               "master_instance_id", String.to_json f)
         ; Util.option_map v.master_public_dns_name (fun f ->
               "master_public_dns_name", String.to_json f)
         ; Some ("master_instance_type", String.to_json v.master_instance_type)
         ])

  let of_json j =
    { master_instance_type =
        String.of_json (Util.of_option_exn (Json.lookup j "master_instance_type"))
    ; master_public_dns_name =
        Util.option_map (Json.lookup j "master_public_dns_name") String.of_json
    ; master_instance_id =
        Util.option_map (Json.lookup j "master_instance_id") String.of_json
    ; slave_instance_type =
        String.of_json (Util.of_option_exn (Json.lookup j "slave_instance_type"))
    ; instance_count =
        Integer.of_json (Util.of_option_exn (Json.lookup j "instance_count"))
    ; instance_groups =
        InstanceGroupDetailList.of_json
          (Util.of_option_exn (Json.lookup j "instance_groups"))
    ; normalized_instance_hours =
        Util.option_map (Json.lookup j "normalized_instance_hours") Integer.of_json
    ; ec2_key_name = Util.option_map (Json.lookup j "ec2_key_name") String.of_json
    ; ec2_subnet_id = Util.option_map (Json.lookup j "ec2_subnet_id") String.of_json
    ; placement = Util.option_map (Json.lookup j "placement") PlacementType.of_json
    ; keep_job_flow_alive_when_no_steps =
        Util.option_map
          (Json.lookup j "keep_job_flow_alive_when_no_steps")
          Boolean.of_json
    ; termination_protected =
        Util.option_map (Json.lookup j "termination_protected") Boolean.of_json
    ; hadoop_version = Util.option_map (Json.lookup j "hadoop_version") String.of_json
    }
end

module ScaleDownBehavior = struct
  type t =
    | TERMINATE_AT_INSTANCE_HOUR
    | TERMINATE_AT_TASK_COMPLETION

  let str_to_t =
    [ "TERMINATE_AT_TASK_COMPLETION", TERMINATE_AT_TASK_COMPLETION
    ; "TERMINATE_AT_INSTANCE_HOUR", TERMINATE_AT_INSTANCE_HOUR
    ]

  let t_to_str =
    [ TERMINATE_AT_TASK_COMPLETION, "TERMINATE_AT_TASK_COMPLETION"
    ; TERMINATE_AT_INSTANCE_HOUR, "TERMINATE_AT_INSTANCE_HOUR"
    ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module StepDetailList = struct
  type t = StepDetail.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map StepDetail.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list StepDetail.to_query v

  let to_json v = `List (List.map StepDetail.to_json v)

  let of_json j = Json.to_list StepDetail.of_json j
end

module SupportedProductsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module CancelStepsRequestStatus = struct
  type t =
    | SUBMITTED
    | FAILED

  let str_to_t = [ "FAILED", FAILED; "SUBMITTED", SUBMITTED ]

  let t_to_str = [ FAILED, "FAILED"; SUBMITTED, "SUBMITTED" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module ApplicationList = struct
  type t = Application.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Application.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Application.to_query v

  let to_json v = `List (List.map Application.to_json v)

  let of_json j = Json.to_list Application.of_json j
end

module Ec2InstanceAttributes = struct
  type t =
    { ec2_key_name : String.t option
    ; ec2_subnet_id : String.t option
    ; requested_ec2_subnet_ids : XmlStringMaxLen256List.t
    ; ec2_availability_zone : String.t option
    ; requested_ec2_availability_zones : XmlStringMaxLen256List.t
    ; iam_instance_profile : String.t option
    ; emr_managed_master_security_group : String.t option
    ; emr_managed_slave_security_group : String.t option
    ; service_access_security_group : String.t option
    ; additional_master_security_groups : StringList.t
    ; additional_slave_security_groups : StringList.t
    }

  let make
      ?ec2_key_name
      ?ec2_subnet_id
      ?(requested_ec2_subnet_ids = [])
      ?ec2_availability_zone
      ?(requested_ec2_availability_zones = [])
      ?iam_instance_profile
      ?emr_managed_master_security_group
      ?emr_managed_slave_security_group
      ?service_access_security_group
      ?(additional_master_security_groups = [])
      ?(additional_slave_security_groups = [])
      () =
    { ec2_key_name
    ; ec2_subnet_id
    ; requested_ec2_subnet_ids
    ; ec2_availability_zone
    ; requested_ec2_availability_zones
    ; iam_instance_profile
    ; emr_managed_master_security_group
    ; emr_managed_slave_security_group
    ; service_access_security_group
    ; additional_master_security_groups
    ; additional_slave_security_groups
    }

  let parse xml =
    Some
      { ec2_key_name = Util.option_bind (Xml.member "Ec2KeyName" xml) String.parse
      ; ec2_subnet_id = Util.option_bind (Xml.member "Ec2SubnetId" xml) String.parse
      ; requested_ec2_subnet_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "RequestedEc2SubnetIds" xml)
               XmlStringMaxLen256List.parse)
      ; ec2_availability_zone =
          Util.option_bind (Xml.member "Ec2AvailabilityZone" xml) String.parse
      ; requested_ec2_availability_zones =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "RequestedEc2AvailabilityZones" xml)
               XmlStringMaxLen256List.parse)
      ; iam_instance_profile =
          Util.option_bind (Xml.member "IamInstanceProfile" xml) String.parse
      ; emr_managed_master_security_group =
          Util.option_bind (Xml.member "EmrManagedMasterSecurityGroup" xml) String.parse
      ; emr_managed_slave_security_group =
          Util.option_bind (Xml.member "EmrManagedSlaveSecurityGroup" xml) String.parse
      ; service_access_security_group =
          Util.option_bind (Xml.member "ServiceAccessSecurityGroup" xml) String.parse
      ; additional_master_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AdditionalMasterSecurityGroups" xml)
               StringList.parse)
      ; additional_slave_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AdditionalSlaveSecurityGroups" xml)
               StringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "AdditionalSlaveSecurityGroups.member"
                , StringList.to_query v.additional_slave_security_groups ))
         ; Some
             (Query.Pair
                ( "AdditionalMasterSecurityGroups.member"
                , StringList.to_query v.additional_master_security_groups ))
         ; Util.option_map v.service_access_security_group (fun f ->
               Query.Pair ("ServiceAccessSecurityGroup", String.to_query f))
         ; Util.option_map v.emr_managed_slave_security_group (fun f ->
               Query.Pair ("EmrManagedSlaveSecurityGroup", String.to_query f))
         ; Util.option_map v.emr_managed_master_security_group (fun f ->
               Query.Pair ("EmrManagedMasterSecurityGroup", String.to_query f))
         ; Util.option_map v.iam_instance_profile (fun f ->
               Query.Pair ("IamInstanceProfile", String.to_query f))
         ; Some
             (Query.Pair
                ( "RequestedEc2AvailabilityZones.member"
                , XmlStringMaxLen256List.to_query v.requested_ec2_availability_zones ))
         ; Util.option_map v.ec2_availability_zone (fun f ->
               Query.Pair ("Ec2AvailabilityZone", String.to_query f))
         ; Some
             (Query.Pair
                ( "RequestedEc2SubnetIds.member"
                , XmlStringMaxLen256List.to_query v.requested_ec2_subnet_ids ))
         ; Util.option_map v.ec2_subnet_id (fun f ->
               Query.Pair ("Ec2SubnetId", String.to_query f))
         ; Util.option_map v.ec2_key_name (fun f ->
               Query.Pair ("Ec2KeyName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "additional_slave_security_groups"
             , StringList.to_json v.additional_slave_security_groups )
         ; Some
             ( "additional_master_security_groups"
             , StringList.to_json v.additional_master_security_groups )
         ; Util.option_map v.service_access_security_group (fun f ->
               "service_access_security_group", String.to_json f)
         ; Util.option_map v.emr_managed_slave_security_group (fun f ->
               "emr_managed_slave_security_group", String.to_json f)
         ; Util.option_map v.emr_managed_master_security_group (fun f ->
               "emr_managed_master_security_group", String.to_json f)
         ; Util.option_map v.iam_instance_profile (fun f ->
               "iam_instance_profile", String.to_json f)
         ; Some
             ( "requested_ec2_availability_zones"
             , XmlStringMaxLen256List.to_json v.requested_ec2_availability_zones )
         ; Util.option_map v.ec2_availability_zone (fun f ->
               "ec2_availability_zone", String.to_json f)
         ; Some
             ( "requested_ec2_subnet_ids"
             , XmlStringMaxLen256List.to_json v.requested_ec2_subnet_ids )
         ; Util.option_map v.ec2_subnet_id (fun f -> "ec2_subnet_id", String.to_json f)
         ; Util.option_map v.ec2_key_name (fun f -> "ec2_key_name", String.to_json f)
         ])

  let of_json j =
    { ec2_key_name = Util.option_map (Json.lookup j "ec2_key_name") String.of_json
    ; ec2_subnet_id = Util.option_map (Json.lookup j "ec2_subnet_id") String.of_json
    ; requested_ec2_subnet_ids =
        XmlStringMaxLen256List.of_json
          (Util.of_option_exn (Json.lookup j "requested_ec2_subnet_ids"))
    ; ec2_availability_zone =
        Util.option_map (Json.lookup j "ec2_availability_zone") String.of_json
    ; requested_ec2_availability_zones =
        XmlStringMaxLen256List.of_json
          (Util.of_option_exn (Json.lookup j "requested_ec2_availability_zones"))
    ; iam_instance_profile =
        Util.option_map (Json.lookup j "iam_instance_profile") String.of_json
    ; emr_managed_master_security_group =
        Util.option_map (Json.lookup j "emr_managed_master_security_group") String.of_json
    ; emr_managed_slave_security_group =
        Util.option_map (Json.lookup j "emr_managed_slave_security_group") String.of_json
    ; service_access_security_group =
        Util.option_map (Json.lookup j "service_access_security_group") String.of_json
    ; additional_master_security_groups =
        StringList.of_json
          (Util.of_option_exn (Json.lookup j "additional_master_security_groups"))
    ; additional_slave_security_groups =
        StringList.of_json
          (Util.of_option_exn (Json.lookup j "additional_slave_security_groups"))
    }
end

module InstanceCollectionType = struct
  type t =
    | INSTANCE_FLEET
    | INSTANCE_GROUP

  let str_to_t = [ "INSTANCE_GROUP", INSTANCE_GROUP; "INSTANCE_FLEET", INSTANCE_FLEET ]

  let t_to_str = [ INSTANCE_GROUP, "INSTANCE_GROUP"; INSTANCE_FLEET, "INSTANCE_FLEET" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module KerberosAttributes = struct
  type t =
    { realm : String.t
    ; kdc_admin_password : String.t
    ; cross_realm_trust_principal_password : String.t option
    ; a_d_domain_join_user : String.t option
    ; a_d_domain_join_password : String.t option
    }

  let make
      ~realm
      ~kdc_admin_password
      ?cross_realm_trust_principal_password
      ?a_d_domain_join_user
      ?a_d_domain_join_password
      () =
    { realm
    ; kdc_admin_password
    ; cross_realm_trust_principal_password
    ; a_d_domain_join_user
    ; a_d_domain_join_password
    }

  let parse xml =
    Some
      { realm =
          Xml.required "Realm" (Util.option_bind (Xml.member "Realm" xml) String.parse)
      ; kdc_admin_password =
          Xml.required
            "KdcAdminPassword"
            (Util.option_bind (Xml.member "KdcAdminPassword" xml) String.parse)
      ; cross_realm_trust_principal_password =
          Util.option_bind
            (Xml.member "CrossRealmTrustPrincipalPassword" xml)
            String.parse
      ; a_d_domain_join_user =
          Util.option_bind (Xml.member "ADDomainJoinUser" xml) String.parse
      ; a_d_domain_join_password =
          Util.option_bind (Xml.member "ADDomainJoinPassword" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.a_d_domain_join_password (fun f ->
               Query.Pair ("ADDomainJoinPassword", String.to_query f))
         ; Util.option_map v.a_d_domain_join_user (fun f ->
               Query.Pair ("ADDomainJoinUser", String.to_query f))
         ; Util.option_map v.cross_realm_trust_principal_password (fun f ->
               Query.Pair ("CrossRealmTrustPrincipalPassword", String.to_query f))
         ; Some (Query.Pair ("KdcAdminPassword", String.to_query v.kdc_admin_password))
         ; Some (Query.Pair ("Realm", String.to_query v.realm))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.a_d_domain_join_password (fun f ->
               "a_d_domain_join_password", String.to_json f)
         ; Util.option_map v.a_d_domain_join_user (fun f ->
               "a_d_domain_join_user", String.to_json f)
         ; Util.option_map v.cross_realm_trust_principal_password (fun f ->
               "cross_realm_trust_principal_password", String.to_json f)
         ; Some ("kdc_admin_password", String.to_json v.kdc_admin_password)
         ; Some ("realm", String.to_json v.realm)
         ])

  let of_json j =
    { realm = String.of_json (Util.of_option_exn (Json.lookup j "realm"))
    ; kdc_admin_password =
        String.of_json (Util.of_option_exn (Json.lookup j "kdc_admin_password"))
    ; cross_realm_trust_principal_password =
        Util.option_map
          (Json.lookup j "cross_realm_trust_principal_password")
          String.of_json
    ; a_d_domain_join_user =
        Util.option_map (Json.lookup j "a_d_domain_join_user") String.of_json
    ; a_d_domain_join_password =
        Util.option_map (Json.lookup j "a_d_domain_join_password") String.of_json
    }
end

module RepoUpgradeOnBoot = struct
  type t =
    | SECURITY
    | NONE

  let str_to_t = [ "NONE", NONE; "SECURITY", SECURITY ]

  let t_to_str = [ NONE, "NONE"; SECURITY, "SECURITY" ]

  let to_string e = Util.of_option_exn (Util.list_find t_to_str e)

  let of_string s = Util.of_option_exn (Util.list_find str_to_t s)

  let make v () = v

  let parse xml = Util.option_bind (String.parse xml) (fun s -> Util.list_find str_to_t s)

  let to_query v = Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))

  let to_json v = String.to_json (Util.of_option_exn (Util.list_find t_to_str v))

  let of_json j = Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
end

module TagList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Tag.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Json.to_list Tag.of_json j
end

module InstanceFleetConfigList = struct
  type t = InstanceFleetConfig.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceFleetConfig.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceFleetConfig.to_query v

  let to_json v = `List (List.map InstanceFleetConfig.to_json v)

  let of_json j = Json.to_list InstanceFleetConfig.of_json j
end

module InstanceGroupConfigList = struct
  type t = InstanceGroupConfig.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceGroupConfig.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceGroupConfig.to_query v

  let to_json v = `List (List.map InstanceGroupConfig.to_json v)

  let of_json j = Json.to_list InstanceGroupConfig.of_json j
end

module SecurityGroupsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module SupportedProductConfig = struct
  type t =
    { name : String.t option
    ; args : XmlStringList.t
    }

  let make ?name ?(args = []) () = { name; args }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; args =
          Util.of_option [] (Util.option_bind (Xml.member "Args" xml) XmlStringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Args.member", XmlStringList.to_query v.args))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("args", XmlStringList.to_json v.args)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; args = XmlStringList.of_json (Util.of_option_exn (Json.lookup j "args"))
    }
end

module SecurityConfigurationSummary = struct
  type t =
    { name : String.t option
    ; creation_date_time : DateTime.t option
    }

  let make ?name ?creation_date_time () = { name; creation_date_time }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; creation_date_time =
          Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.creation_date_time (fun f ->
               Query.Pair ("CreationDateTime", DateTime.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.creation_date_time (fun f ->
               "creation_date_time", DateTime.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; creation_date_time =
        Util.option_map (Json.lookup j "creation_date_time") DateTime.of_json
    }
end

module InstanceFleet = struct
  type t =
    { id : String.t option
    ; name : String.t option
    ; status : InstanceFleetStatus.t option
    ; instance_fleet_type : InstanceFleetType.t option
    ; target_on_demand_capacity : Integer.t option
    ; target_spot_capacity : Integer.t option
    ; provisioned_on_demand_capacity : Integer.t option
    ; provisioned_spot_capacity : Integer.t option
    ; instance_type_specifications : InstanceTypeSpecificationList.t
    ; launch_specifications : InstanceFleetProvisioningSpecifications.t option
    }

  let make
      ?id
      ?name
      ?status
      ?instance_fleet_type
      ?target_on_demand_capacity
      ?target_spot_capacity
      ?provisioned_on_demand_capacity
      ?provisioned_spot_capacity
      ?(instance_type_specifications = [])
      ?launch_specifications
      () =
    { id
    ; name
    ; status
    ; instance_fleet_type
    ; target_on_demand_capacity
    ; target_spot_capacity
    ; provisioned_on_demand_capacity
    ; provisioned_spot_capacity
    ; instance_type_specifications
    ; launch_specifications
    }

  let parse xml =
    Some
      { id = Util.option_bind (Xml.member "Id" xml) String.parse
      ; name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) InstanceFleetStatus.parse
      ; instance_fleet_type =
          Util.option_bind (Xml.member "InstanceFleetType" xml) InstanceFleetType.parse
      ; target_on_demand_capacity =
          Util.option_bind (Xml.member "TargetOnDemandCapacity" xml) Integer.parse
      ; target_spot_capacity =
          Util.option_bind (Xml.member "TargetSpotCapacity" xml) Integer.parse
      ; provisioned_on_demand_capacity =
          Util.option_bind (Xml.member "ProvisionedOnDemandCapacity" xml) Integer.parse
      ; provisioned_spot_capacity =
          Util.option_bind (Xml.member "ProvisionedSpotCapacity" xml) Integer.parse
      ; instance_type_specifications =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstanceTypeSpecifications" xml)
               InstanceTypeSpecificationList.parse)
      ; launch_specifications =
          Util.option_bind
            (Xml.member "LaunchSpecifications" xml)
            InstanceFleetProvisioningSpecifications.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.launch_specifications (fun f ->
               Query.Pair
                 ( "LaunchSpecifications"
                 , InstanceFleetProvisioningSpecifications.to_query f ))
         ; Some
             (Query.Pair
                ( "InstanceTypeSpecifications.member"
                , InstanceTypeSpecificationList.to_query v.instance_type_specifications ))
         ; Util.option_map v.provisioned_spot_capacity (fun f ->
               Query.Pair ("ProvisionedSpotCapacity", Integer.to_query f))
         ; Util.option_map v.provisioned_on_demand_capacity (fun f ->
               Query.Pair ("ProvisionedOnDemandCapacity", Integer.to_query f))
         ; Util.option_map v.target_spot_capacity (fun f ->
               Query.Pair ("TargetSpotCapacity", Integer.to_query f))
         ; Util.option_map v.target_on_demand_capacity (fun f ->
               Query.Pair ("TargetOnDemandCapacity", Integer.to_query f))
         ; Util.option_map v.instance_fleet_type (fun f ->
               Query.Pair ("InstanceFleetType", InstanceFleetType.to_query f))
         ; Util.option_map v.status (fun f ->
               Query.Pair ("Status", InstanceFleetStatus.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ; Util.option_map v.id (fun f -> Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.launch_specifications (fun f ->
               "launch_specifications", InstanceFleetProvisioningSpecifications.to_json f)
         ; Some
             ( "instance_type_specifications"
             , InstanceTypeSpecificationList.to_json v.instance_type_specifications )
         ; Util.option_map v.provisioned_spot_capacity (fun f ->
               "provisioned_spot_capacity", Integer.to_json f)
         ; Util.option_map v.provisioned_on_demand_capacity (fun f ->
               "provisioned_on_demand_capacity", Integer.to_json f)
         ; Util.option_map v.target_spot_capacity (fun f ->
               "target_spot_capacity", Integer.to_json f)
         ; Util.option_map v.target_on_demand_capacity (fun f ->
               "target_on_demand_capacity", Integer.to_json f)
         ; Util.option_map v.instance_fleet_type (fun f ->
               "instance_fleet_type", InstanceFleetType.to_json f)
         ; Util.option_map v.status (fun f -> "status", InstanceFleetStatus.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ; Util.option_map v.id (fun f -> "id", String.to_json f)
         ])

  let of_json j =
    { id = Util.option_map (Json.lookup j "id") String.of_json
    ; name = Util.option_map (Json.lookup j "name") String.of_json
    ; status = Util.option_map (Json.lookup j "status") InstanceFleetStatus.of_json
    ; instance_fleet_type =
        Util.option_map (Json.lookup j "instance_fleet_type") InstanceFleetType.of_json
    ; target_on_demand_capacity =
        Util.option_map (Json.lookup j "target_on_demand_capacity") Integer.of_json
    ; target_spot_capacity =
        Util.option_map (Json.lookup j "target_spot_capacity") Integer.of_json
    ; provisioned_on_demand_capacity =
        Util.option_map (Json.lookup j "provisioned_on_demand_capacity") Integer.of_json
    ; provisioned_spot_capacity =
        Util.option_map (Json.lookup j "provisioned_spot_capacity") Integer.of_json
    ; instance_type_specifications =
        InstanceTypeSpecificationList.of_json
          (Util.of_option_exn (Json.lookup j "instance_type_specifications"))
    ; launch_specifications =
        Util.option_map
          (Json.lookup j "launch_specifications")
          InstanceFleetProvisioningSpecifications.of_json
    }
end

module StepSummary = struct
  type t =
    { id : String.t option
    ; name : String.t option
    ; config : HadoopStepConfig.t option
    ; action_on_failure : ActionOnFailure.t option
    ; status : StepStatus.t option
    }

  let make ?id ?name ?config ?action_on_failure ?status () =
    { id; name; config; action_on_failure; status }

  let parse xml =
    Some
      { id = Util.option_bind (Xml.member "Id" xml) String.parse
      ; name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; config = Util.option_bind (Xml.member "Config" xml) HadoopStepConfig.parse
      ; action_on_failure =
          Util.option_bind (Xml.member "ActionOnFailure" xml) ActionOnFailure.parse
      ; status = Util.option_bind (Xml.member "Status" xml) StepStatus.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f ->
               Query.Pair ("Status", StepStatus.to_query f))
         ; Util.option_map v.action_on_failure (fun f ->
               Query.Pair ("ActionOnFailure", ActionOnFailure.to_query f))
         ; Util.option_map v.config (fun f ->
               Query.Pair ("Config", HadoopStepConfig.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ; Util.option_map v.id (fun f -> Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", StepStatus.to_json f)
         ; Util.option_map v.action_on_failure (fun f ->
               "action_on_failure", ActionOnFailure.to_json f)
         ; Util.option_map v.config (fun f -> "config", HadoopStepConfig.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ; Util.option_map v.id (fun f -> "id", String.to_json f)
         ])

  let of_json j =
    { id = Util.option_map (Json.lookup j "id") String.of_json
    ; name = Util.option_map (Json.lookup j "name") String.of_json
    ; config = Util.option_map (Json.lookup j "config") HadoopStepConfig.of_json
    ; action_on_failure =
        Util.option_map (Json.lookup j "action_on_failure") ActionOnFailure.of_json
    ; status = Util.option_map (Json.lookup j "status") StepStatus.of_json
    }
end

module InstanceGroupModifyConfig = struct
  type t =
    { instance_group_id : String.t
    ; instance_count : Integer.t option
    ; e_c2_instance_ids_to_terminate : EC2InstanceIdsToTerminateList.t
    ; shrink_policy : ShrinkPolicy.t option
    }

  let make
      ~instance_group_id
      ?instance_count
      ?(e_c2_instance_ids_to_terminate = [])
      ?shrink_policy
      () =
    { instance_group_id; instance_count; e_c2_instance_ids_to_terminate; shrink_policy }

  let parse xml =
    Some
      { instance_group_id =
          Xml.required
            "InstanceGroupId"
            (Util.option_bind (Xml.member "InstanceGroupId" xml) String.parse)
      ; instance_count = Util.option_bind (Xml.member "InstanceCount" xml) Integer.parse
      ; e_c2_instance_ids_to_terminate =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "EC2InstanceIdsToTerminate" xml)
               EC2InstanceIdsToTerminateList.parse)
      ; shrink_policy =
          Util.option_bind (Xml.member "ShrinkPolicy" xml) ShrinkPolicy.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.shrink_policy (fun f ->
               Query.Pair ("ShrinkPolicy", ShrinkPolicy.to_query f))
         ; Some
             (Query.Pair
                ( "EC2InstanceIdsToTerminate.member"
                , EC2InstanceIdsToTerminateList.to_query v.e_c2_instance_ids_to_terminate
                ))
         ; Util.option_map v.instance_count (fun f ->
               Query.Pair ("InstanceCount", Integer.to_query f))
         ; Some (Query.Pair ("InstanceGroupId", String.to_query v.instance_group_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.shrink_policy (fun f ->
               "shrink_policy", ShrinkPolicy.to_json f)
         ; Some
             ( "e_c2_instance_ids_to_terminate"
             , EC2InstanceIdsToTerminateList.to_json v.e_c2_instance_ids_to_terminate )
         ; Util.option_map v.instance_count (fun f -> "instance_count", Integer.to_json f)
         ; Some ("instance_group_id", String.to_json v.instance_group_id)
         ])

  let of_json j =
    { instance_group_id =
        String.of_json (Util.of_option_exn (Json.lookup j "instance_group_id"))
    ; instance_count = Util.option_map (Json.lookup j "instance_count") Integer.of_json
    ; e_c2_instance_ids_to_terminate =
        EC2InstanceIdsToTerminateList.of_json
          (Util.of_option_exn (Json.lookup j "e_c2_instance_ids_to_terminate"))
    ; shrink_policy = Util.option_map (Json.lookup j "shrink_policy") ShrinkPolicy.of_json
    }
end

module InstanceGroup = struct
  type t =
    { id : String.t option
    ; name : String.t option
    ; market : MarketType.t option
    ; instance_group_type : InstanceGroupType.t option
    ; bid_price : String.t option
    ; instance_type : String.t option
    ; requested_instance_count : Integer.t option
    ; running_instance_count : Integer.t option
    ; status : InstanceGroupStatus.t option
    ; configurations : ConfigurationList.t
    ; ebs_block_devices : EbsBlockDeviceList.t
    ; ebs_optimized : Boolean.t option
    ; shrink_policy : ShrinkPolicy.t option
    ; auto_scaling_policy : AutoScalingPolicyDescription.t option
    }

  let make
      ?id
      ?name
      ?market
      ?instance_group_type
      ?bid_price
      ?instance_type
      ?requested_instance_count
      ?running_instance_count
      ?status
      ?(configurations = [])
      ?(ebs_block_devices = [])
      ?ebs_optimized
      ?shrink_policy
      ?auto_scaling_policy
      () =
    { id
    ; name
    ; market
    ; instance_group_type
    ; bid_price
    ; instance_type
    ; requested_instance_count
    ; running_instance_count
    ; status
    ; configurations
    ; ebs_block_devices
    ; ebs_optimized
    ; shrink_policy
    ; auto_scaling_policy
    }

  let parse xml =
    Some
      { id = Util.option_bind (Xml.member "Id" xml) String.parse
      ; name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; market = Util.option_bind (Xml.member "Market" xml) MarketType.parse
      ; instance_group_type =
          Util.option_bind (Xml.member "InstanceGroupType" xml) InstanceGroupType.parse
      ; bid_price = Util.option_bind (Xml.member "BidPrice" xml) String.parse
      ; instance_type = Util.option_bind (Xml.member "InstanceType" xml) String.parse
      ; requested_instance_count =
          Util.option_bind (Xml.member "RequestedInstanceCount" xml) Integer.parse
      ; running_instance_count =
          Util.option_bind (Xml.member "RunningInstanceCount" xml) Integer.parse
      ; status = Util.option_bind (Xml.member "Status" xml) InstanceGroupStatus.parse
      ; configurations =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Configurations" xml) ConfigurationList.parse)
      ; ebs_block_devices =
          Util.of_option
            []
            (Util.option_bind (Xml.member "EbsBlockDevices" xml) EbsBlockDeviceList.parse)
      ; ebs_optimized = Util.option_bind (Xml.member "EbsOptimized" xml) Boolean.parse
      ; shrink_policy =
          Util.option_bind (Xml.member "ShrinkPolicy" xml) ShrinkPolicy.parse
      ; auto_scaling_policy =
          Util.option_bind
            (Xml.member "AutoScalingPolicy" xml)
            AutoScalingPolicyDescription.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.auto_scaling_policy (fun f ->
               Query.Pair ("AutoScalingPolicy", AutoScalingPolicyDescription.to_query f))
         ; Util.option_map v.shrink_policy (fun f ->
               Query.Pair ("ShrinkPolicy", ShrinkPolicy.to_query f))
         ; Util.option_map v.ebs_optimized (fun f ->
               Query.Pair ("EbsOptimized", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("EbsBlockDevices.member", EbsBlockDeviceList.to_query v.ebs_block_devices))
         ; Some
             (Query.Pair
                ("Configurations.member", ConfigurationList.to_query v.configurations))
         ; Util.option_map v.status (fun f ->
               Query.Pair ("Status", InstanceGroupStatus.to_query f))
         ; Util.option_map v.running_instance_count (fun f ->
               Query.Pair ("RunningInstanceCount", Integer.to_query f))
         ; Util.option_map v.requested_instance_count (fun f ->
               Query.Pair ("RequestedInstanceCount", Integer.to_query f))
         ; Util.option_map v.instance_type (fun f ->
               Query.Pair ("InstanceType", String.to_query f))
         ; Util.option_map v.bid_price (fun f ->
               Query.Pair ("BidPrice", String.to_query f))
         ; Util.option_map v.instance_group_type (fun f ->
               Query.Pair ("InstanceGroupType", InstanceGroupType.to_query f))
         ; Util.option_map v.market (fun f ->
               Query.Pair ("Market", MarketType.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ; Util.option_map v.id (fun f -> Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.auto_scaling_policy (fun f ->
               "auto_scaling_policy", AutoScalingPolicyDescription.to_json f)
         ; Util.option_map v.shrink_policy (fun f ->
               "shrink_policy", ShrinkPolicy.to_json f)
         ; Util.option_map v.ebs_optimized (fun f -> "ebs_optimized", Boolean.to_json f)
         ; Some ("ebs_block_devices", EbsBlockDeviceList.to_json v.ebs_block_devices)
         ; Some ("configurations", ConfigurationList.to_json v.configurations)
         ; Util.option_map v.status (fun f -> "status", InstanceGroupStatus.to_json f)
         ; Util.option_map v.running_instance_count (fun f ->
               "running_instance_count", Integer.to_json f)
         ; Util.option_map v.requested_instance_count (fun f ->
               "requested_instance_count", Integer.to_json f)
         ; Util.option_map v.instance_type (fun f -> "instance_type", String.to_json f)
         ; Util.option_map v.bid_price (fun f -> "bid_price", String.to_json f)
         ; Util.option_map v.instance_group_type (fun f ->
               "instance_group_type", InstanceGroupType.to_json f)
         ; Util.option_map v.market (fun f -> "market", MarketType.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ; Util.option_map v.id (fun f -> "id", String.to_json f)
         ])

  let of_json j =
    { id = Util.option_map (Json.lookup j "id") String.of_json
    ; name = Util.option_map (Json.lookup j "name") String.of_json
    ; market = Util.option_map (Json.lookup j "market") MarketType.of_json
    ; instance_group_type =
        Util.option_map (Json.lookup j "instance_group_type") InstanceGroupType.of_json
    ; bid_price = Util.option_map (Json.lookup j "bid_price") String.of_json
    ; instance_type = Util.option_map (Json.lookup j "instance_type") String.of_json
    ; requested_instance_count =
        Util.option_map (Json.lookup j "requested_instance_count") Integer.of_json
    ; running_instance_count =
        Util.option_map (Json.lookup j "running_instance_count") Integer.of_json
    ; status = Util.option_map (Json.lookup j "status") InstanceGroupStatus.of_json
    ; configurations =
        ConfigurationList.of_json (Util.of_option_exn (Json.lookup j "configurations"))
    ; ebs_block_devices =
        EbsBlockDeviceList.of_json
          (Util.of_option_exn (Json.lookup j "ebs_block_devices"))
    ; ebs_optimized = Util.option_map (Json.lookup j "ebs_optimized") Boolean.of_json
    ; shrink_policy = Util.option_map (Json.lookup j "shrink_policy") ShrinkPolicy.of_json
    ; auto_scaling_policy =
        Util.option_map
          (Json.lookup j "auto_scaling_policy")
          AutoScalingPolicyDescription.of_json
    }
end

module Instance = struct
  type t =
    { id : String.t option
    ; ec2_instance_id : String.t option
    ; public_dns_name : String.t option
    ; public_ip_address : String.t option
    ; private_dns_name : String.t option
    ; private_ip_address : String.t option
    ; status : InstanceStatus.t option
    ; instance_group_id : String.t option
    ; instance_fleet_id : String.t option
    ; market : MarketType.t option
    ; instance_type : String.t option
    ; ebs_volumes : EbsVolumeList.t
    }

  let make
      ?id
      ?ec2_instance_id
      ?public_dns_name
      ?public_ip_address
      ?private_dns_name
      ?private_ip_address
      ?status
      ?instance_group_id
      ?instance_fleet_id
      ?market
      ?instance_type
      ?(ebs_volumes = [])
      () =
    { id
    ; ec2_instance_id
    ; public_dns_name
    ; public_ip_address
    ; private_dns_name
    ; private_ip_address
    ; status
    ; instance_group_id
    ; instance_fleet_id
    ; market
    ; instance_type
    ; ebs_volumes
    }

  let parse xml =
    Some
      { id = Util.option_bind (Xml.member "Id" xml) String.parse
      ; ec2_instance_id = Util.option_bind (Xml.member "Ec2InstanceId" xml) String.parse
      ; public_dns_name = Util.option_bind (Xml.member "PublicDnsName" xml) String.parse
      ; public_ip_address =
          Util.option_bind (Xml.member "PublicIpAddress" xml) String.parse
      ; private_dns_name = Util.option_bind (Xml.member "PrivateDnsName" xml) String.parse
      ; private_ip_address =
          Util.option_bind (Xml.member "PrivateIpAddress" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) InstanceStatus.parse
      ; instance_group_id =
          Util.option_bind (Xml.member "InstanceGroupId" xml) String.parse
      ; instance_fleet_id =
          Util.option_bind (Xml.member "InstanceFleetId" xml) String.parse
      ; market = Util.option_bind (Xml.member "Market" xml) MarketType.parse
      ; instance_type = Util.option_bind (Xml.member "InstanceType" xml) String.parse
      ; ebs_volumes =
          Util.of_option
            []
            (Util.option_bind (Xml.member "EbsVolumes" xml) EbsVolumeList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("EbsVolumes.member", EbsVolumeList.to_query v.ebs_volumes))
         ; Util.option_map v.instance_type (fun f ->
               Query.Pair ("InstanceType", String.to_query f))
         ; Util.option_map v.market (fun f ->
               Query.Pair ("Market", MarketType.to_query f))
         ; Util.option_map v.instance_fleet_id (fun f ->
               Query.Pair ("InstanceFleetId", String.to_query f))
         ; Util.option_map v.instance_group_id (fun f ->
               Query.Pair ("InstanceGroupId", String.to_query f))
         ; Util.option_map v.status (fun f ->
               Query.Pair ("Status", InstanceStatus.to_query f))
         ; Util.option_map v.private_ip_address (fun f ->
               Query.Pair ("PrivateIpAddress", String.to_query f))
         ; Util.option_map v.private_dns_name (fun f ->
               Query.Pair ("PrivateDnsName", String.to_query f))
         ; Util.option_map v.public_ip_address (fun f ->
               Query.Pair ("PublicIpAddress", String.to_query f))
         ; Util.option_map v.public_dns_name (fun f ->
               Query.Pair ("PublicDnsName", String.to_query f))
         ; Util.option_map v.ec2_instance_id (fun f ->
               Query.Pair ("Ec2InstanceId", String.to_query f))
         ; Util.option_map v.id (fun f -> Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("ebs_volumes", EbsVolumeList.to_json v.ebs_volumes)
         ; Util.option_map v.instance_type (fun f -> "instance_type", String.to_json f)
         ; Util.option_map v.market (fun f -> "market", MarketType.to_json f)
         ; Util.option_map v.instance_fleet_id (fun f ->
               "instance_fleet_id", String.to_json f)
         ; Util.option_map v.instance_group_id (fun f ->
               "instance_group_id", String.to_json f)
         ; Util.option_map v.status (fun f -> "status", InstanceStatus.to_json f)
         ; Util.option_map v.private_ip_address (fun f ->
               "private_ip_address", String.to_json f)
         ; Util.option_map v.private_dns_name (fun f ->
               "private_dns_name", String.to_json f)
         ; Util.option_map v.public_ip_address (fun f ->
               "public_ip_address", String.to_json f)
         ; Util.option_map v.public_dns_name (fun f ->
               "public_dns_name", String.to_json f)
         ; Util.option_map v.ec2_instance_id (fun f ->
               "ec2_instance_id", String.to_json f)
         ; Util.option_map v.id (fun f -> "id", String.to_json f)
         ])

  let of_json j =
    { id = Util.option_map (Json.lookup j "id") String.of_json
    ; ec2_instance_id = Util.option_map (Json.lookup j "ec2_instance_id") String.of_json
    ; public_dns_name = Util.option_map (Json.lookup j "public_dns_name") String.of_json
    ; public_ip_address =
        Util.option_map (Json.lookup j "public_ip_address") String.of_json
    ; private_dns_name = Util.option_map (Json.lookup j "private_dns_name") String.of_json
    ; private_ip_address =
        Util.option_map (Json.lookup j "private_ip_address") String.of_json
    ; status = Util.option_map (Json.lookup j "status") InstanceStatus.of_json
    ; instance_group_id =
        Util.option_map (Json.lookup j "instance_group_id") String.of_json
    ; instance_fleet_id =
        Util.option_map (Json.lookup j "instance_fleet_id") String.of_json
    ; market = Util.option_map (Json.lookup j "market") MarketType.of_json
    ; instance_type = Util.option_map (Json.lookup j "instance_type") String.of_json
    ; ebs_volumes =
        EbsVolumeList.of_json (Util.of_option_exn (Json.lookup j "ebs_volumes"))
    }
end

module ClusterSummary = struct
  type t =
    { id : String.t option
    ; name : String.t option
    ; status : ClusterStatus.t option
    ; normalized_instance_hours : Integer.t option
    }

  let make ?id ?name ?status ?normalized_instance_hours () =
    { id; name; status; normalized_instance_hours }

  let parse xml =
    Some
      { id = Util.option_bind (Xml.member "Id" xml) String.parse
      ; name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) ClusterStatus.parse
      ; normalized_instance_hours =
          Util.option_bind (Xml.member "NormalizedInstanceHours" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.normalized_instance_hours (fun f ->
               Query.Pair ("NormalizedInstanceHours", Integer.to_query f))
         ; Util.option_map v.status (fun f ->
               Query.Pair ("Status", ClusterStatus.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ; Util.option_map v.id (fun f -> Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.normalized_instance_hours (fun f ->
               "normalized_instance_hours", Integer.to_json f)
         ; Util.option_map v.status (fun f -> "status", ClusterStatus.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ; Util.option_map v.id (fun f -> "id", String.to_json f)
         ])

  let of_json j =
    { id = Util.option_map (Json.lookup j "id") String.of_json
    ; name = Util.option_map (Json.lookup j "name") String.of_json
    ; status = Util.option_map (Json.lookup j "status") ClusterStatus.of_json
    ; normalized_instance_hours =
        Util.option_map (Json.lookup j "normalized_instance_hours") Integer.of_json
    }
end

module JobFlowDetail = struct
  type t =
    { job_flow_id : String.t
    ; name : String.t
    ; log_uri : String.t option
    ; ami_version : String.t option
    ; execution_status_detail : JobFlowExecutionStatusDetail.t
    ; instances : JobFlowInstancesDetail.t
    ; steps : StepDetailList.t
    ; bootstrap_actions : BootstrapActionDetailList.t
    ; supported_products : SupportedProductsList.t
    ; visible_to_all_users : Boolean.t option
    ; job_flow_role : String.t option
    ; service_role : String.t option
    ; auto_scaling_role : String.t option
    ; scale_down_behavior : ScaleDownBehavior.t option
    }

  let make
      ~job_flow_id
      ~name
      ?log_uri
      ?ami_version
      ~execution_status_detail
      ~instances
      ?(steps = [])
      ?(bootstrap_actions = [])
      ?(supported_products = [])
      ?visible_to_all_users
      ?job_flow_role
      ?service_role
      ?auto_scaling_role
      ?scale_down_behavior
      () =
    { job_flow_id
    ; name
    ; log_uri
    ; ami_version
    ; execution_status_detail
    ; instances
    ; steps
    ; bootstrap_actions
    ; supported_products
    ; visible_to_all_users
    ; job_flow_role
    ; service_role
    ; auto_scaling_role
    ; scale_down_behavior
    }

  let parse xml =
    Some
      { job_flow_id =
          Xml.required
            "JobFlowId"
            (Util.option_bind (Xml.member "JobFlowId" xml) String.parse)
      ; name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; log_uri = Util.option_bind (Xml.member "LogUri" xml) String.parse
      ; ami_version = Util.option_bind (Xml.member "AmiVersion" xml) String.parse
      ; execution_status_detail =
          Xml.required
            "ExecutionStatusDetail"
            (Util.option_bind
               (Xml.member "ExecutionStatusDetail" xml)
               JobFlowExecutionStatusDetail.parse)
      ; instances =
          Xml.required
            "Instances"
            (Util.option_bind (Xml.member "Instances" xml) JobFlowInstancesDetail.parse)
      ; steps =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Steps" xml) StepDetailList.parse)
      ; bootstrap_actions =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "BootstrapActions" xml)
               BootstrapActionDetailList.parse)
      ; supported_products =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SupportedProducts" xml)
               SupportedProductsList.parse)
      ; visible_to_all_users =
          Util.option_bind (Xml.member "VisibleToAllUsers" xml) Boolean.parse
      ; job_flow_role = Util.option_bind (Xml.member "JobFlowRole" xml) String.parse
      ; service_role = Util.option_bind (Xml.member "ServiceRole" xml) String.parse
      ; auto_scaling_role =
          Util.option_bind (Xml.member "AutoScalingRole" xml) String.parse
      ; scale_down_behavior =
          Util.option_bind (Xml.member "ScaleDownBehavior" xml) ScaleDownBehavior.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.scale_down_behavior (fun f ->
               Query.Pair ("ScaleDownBehavior", ScaleDownBehavior.to_query f))
         ; Util.option_map v.auto_scaling_role (fun f ->
               Query.Pair ("AutoScalingRole", String.to_query f))
         ; Util.option_map v.service_role (fun f ->
               Query.Pair ("ServiceRole", String.to_query f))
         ; Util.option_map v.job_flow_role (fun f ->
               Query.Pair ("JobFlowRole", String.to_query f))
         ; Util.option_map v.visible_to_all_users (fun f ->
               Query.Pair ("VisibleToAllUsers", Boolean.to_query f))
         ; Some
             (Query.Pair
                ( "SupportedProducts.member"
                , SupportedProductsList.to_query v.supported_products ))
         ; Some
             (Query.Pair
                ( "BootstrapActions.member"
                , BootstrapActionDetailList.to_query v.bootstrap_actions ))
         ; Some (Query.Pair ("Steps.member", StepDetailList.to_query v.steps))
         ; Some (Query.Pair ("Instances", JobFlowInstancesDetail.to_query v.instances))
         ; Some
             (Query.Pair
                ( "ExecutionStatusDetail"
                , JobFlowExecutionStatusDetail.to_query v.execution_status_detail ))
         ; Util.option_map v.ami_version (fun f ->
               Query.Pair ("AmiVersion", String.to_query f))
         ; Util.option_map v.log_uri (fun f -> Query.Pair ("LogUri", String.to_query f))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ; Some (Query.Pair ("JobFlowId", String.to_query v.job_flow_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.scale_down_behavior (fun f ->
               "scale_down_behavior", ScaleDownBehavior.to_json f)
         ; Util.option_map v.auto_scaling_role (fun f ->
               "auto_scaling_role", String.to_json f)
         ; Util.option_map v.service_role (fun f -> "service_role", String.to_json f)
         ; Util.option_map v.job_flow_role (fun f -> "job_flow_role", String.to_json f)
         ; Util.option_map v.visible_to_all_users (fun f ->
               "visible_to_all_users", Boolean.to_json f)
         ; Some ("supported_products", SupportedProductsList.to_json v.supported_products)
         ; Some
             ("bootstrap_actions", BootstrapActionDetailList.to_json v.bootstrap_actions)
         ; Some ("steps", StepDetailList.to_json v.steps)
         ; Some ("instances", JobFlowInstancesDetail.to_json v.instances)
         ; Some
             ( "execution_status_detail"
             , JobFlowExecutionStatusDetail.to_json v.execution_status_detail )
         ; Util.option_map v.ami_version (fun f -> "ami_version", String.to_json f)
         ; Util.option_map v.log_uri (fun f -> "log_uri", String.to_json f)
         ; Some ("name", String.to_json v.name)
         ; Some ("job_flow_id", String.to_json v.job_flow_id)
         ])

  let of_json j =
    { job_flow_id = String.of_json (Util.of_option_exn (Json.lookup j "job_flow_id"))
    ; name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; log_uri = Util.option_map (Json.lookup j "log_uri") String.of_json
    ; ami_version = Util.option_map (Json.lookup j "ami_version") String.of_json
    ; execution_status_detail =
        JobFlowExecutionStatusDetail.of_json
          (Util.of_option_exn (Json.lookup j "execution_status_detail"))
    ; instances =
        JobFlowInstancesDetail.of_json (Util.of_option_exn (Json.lookup j "instances"))
    ; steps = StepDetailList.of_json (Util.of_option_exn (Json.lookup j "steps"))
    ; bootstrap_actions =
        BootstrapActionDetailList.of_json
          (Util.of_option_exn (Json.lookup j "bootstrap_actions"))
    ; supported_products =
        SupportedProductsList.of_json
          (Util.of_option_exn (Json.lookup j "supported_products"))
    ; visible_to_all_users =
        Util.option_map (Json.lookup j "visible_to_all_users") Boolean.of_json
    ; job_flow_role = Util.option_map (Json.lookup j "job_flow_role") String.of_json
    ; service_role = Util.option_map (Json.lookup j "service_role") String.of_json
    ; auto_scaling_role =
        Util.option_map (Json.lookup j "auto_scaling_role") String.of_json
    ; scale_down_behavior =
        Util.option_map (Json.lookup j "scale_down_behavior") ScaleDownBehavior.of_json
    }
end

module Command = struct
  type t =
    { name : String.t option
    ; script_path : String.t option
    ; args : StringList.t
    }

  let make ?name ?script_path ?(args = []) () = { name; script_path; args }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; script_path = Util.option_bind (Xml.member "ScriptPath" xml) String.parse
      ; args =
          Util.of_option [] (Util.option_bind (Xml.member "Args" xml) StringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Args.member", StringList.to_query v.args))
         ; Util.option_map v.script_path (fun f ->
               Query.Pair ("ScriptPath", String.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("args", StringList.to_json v.args)
         ; Util.option_map v.script_path (fun f -> "script_path", String.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; script_path = Util.option_map (Json.lookup j "script_path") String.of_json
    ; args = StringList.of_json (Util.of_option_exn (Json.lookup j "args"))
    }
end

module CancelStepsInfo = struct
  type t =
    { step_id : String.t option
    ; status : CancelStepsRequestStatus.t option
    ; reason : String.t option
    }

  let make ?step_id ?status ?reason () = { step_id; status; reason }

  let parse xml =
    Some
      { step_id = Util.option_bind (Xml.member "StepId" xml) String.parse
      ; status = Util.option_bind (Xml.member "Status" xml) CancelStepsRequestStatus.parse
      ; reason = Util.option_bind (Xml.member "Reason" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.reason (fun f -> Query.Pair ("Reason", String.to_query f))
         ; Util.option_map v.status (fun f ->
               Query.Pair ("Status", CancelStepsRequestStatus.to_query f))
         ; Util.option_map v.step_id (fun f -> Query.Pair ("StepId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.reason (fun f -> "reason", String.to_json f)
         ; Util.option_map v.status (fun f ->
               "status", CancelStepsRequestStatus.to_json f)
         ; Util.option_map v.step_id (fun f -> "step_id", String.to_json f)
         ])

  let of_json j =
    { step_id = Util.option_map (Json.lookup j "step_id") String.of_json
    ; status = Util.option_map (Json.lookup j "status") CancelStepsRequestStatus.of_json
    ; reason = Util.option_map (Json.lookup j "reason") String.of_json
    }
end

module ClusterStateList = struct
  type t = ClusterState.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map ClusterState.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list ClusterState.to_query v

  let to_json v = `List (List.map ClusterState.to_json v)

  let of_json j = Json.to_list ClusterState.of_json j
end

module InstanceFleetModifyConfig = struct
  type t =
    { instance_fleet_id : String.t
    ; target_on_demand_capacity : Integer.t option
    ; target_spot_capacity : Integer.t option
    }

  let make ~instance_fleet_id ?target_on_demand_capacity ?target_spot_capacity () =
    { instance_fleet_id; target_on_demand_capacity; target_spot_capacity }

  let parse xml =
    Some
      { instance_fleet_id =
          Xml.required
            "InstanceFleetId"
            (Util.option_bind (Xml.member "InstanceFleetId" xml) String.parse)
      ; target_on_demand_capacity =
          Util.option_bind (Xml.member "TargetOnDemandCapacity" xml) Integer.parse
      ; target_spot_capacity =
          Util.option_bind (Xml.member "TargetSpotCapacity" xml) Integer.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.target_spot_capacity (fun f ->
               Query.Pair ("TargetSpotCapacity", Integer.to_query f))
         ; Util.option_map v.target_on_demand_capacity (fun f ->
               Query.Pair ("TargetOnDemandCapacity", Integer.to_query f))
         ; Some (Query.Pair ("InstanceFleetId", String.to_query v.instance_fleet_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.target_spot_capacity (fun f ->
               "target_spot_capacity", Integer.to_json f)
         ; Util.option_map v.target_on_demand_capacity (fun f ->
               "target_on_demand_capacity", Integer.to_json f)
         ; Some ("instance_fleet_id", String.to_json v.instance_fleet_id)
         ])

  let of_json j =
    { instance_fleet_id =
        String.of_json (Util.of_option_exn (Json.lookup j "instance_fleet_id"))
    ; target_on_demand_capacity =
        Util.option_map (Json.lookup j "target_on_demand_capacity") Integer.of_json
    ; target_spot_capacity =
        Util.option_map (Json.lookup j "target_spot_capacity") Integer.of_json
    }
end

module Step = struct
  type t =
    { id : String.t option
    ; name : String.t option
    ; config : HadoopStepConfig.t option
    ; action_on_failure : ActionOnFailure.t option
    ; status : StepStatus.t option
    }

  let make ?id ?name ?config ?action_on_failure ?status () =
    { id; name; config; action_on_failure; status }

  let parse xml =
    Some
      { id = Util.option_bind (Xml.member "Id" xml) String.parse
      ; name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; config = Util.option_bind (Xml.member "Config" xml) HadoopStepConfig.parse
      ; action_on_failure =
          Util.option_bind (Xml.member "ActionOnFailure" xml) ActionOnFailure.parse
      ; status = Util.option_bind (Xml.member "Status" xml) StepStatus.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f ->
               Query.Pair ("Status", StepStatus.to_query f))
         ; Util.option_map v.action_on_failure (fun f ->
               Query.Pair ("ActionOnFailure", ActionOnFailure.to_query f))
         ; Util.option_map v.config (fun f ->
               Query.Pair ("Config", HadoopStepConfig.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ; Util.option_map v.id (fun f -> Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.status (fun f -> "status", StepStatus.to_json f)
         ; Util.option_map v.action_on_failure (fun f ->
               "action_on_failure", ActionOnFailure.to_json f)
         ; Util.option_map v.config (fun f -> "config", HadoopStepConfig.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ; Util.option_map v.id (fun f -> "id", String.to_json f)
         ])

  let of_json j =
    { id = Util.option_map (Json.lookup j "id") String.of_json
    ; name = Util.option_map (Json.lookup j "name") String.of_json
    ; config = Util.option_map (Json.lookup j "config") HadoopStepConfig.of_json
    ; action_on_failure =
        Util.option_map (Json.lookup j "action_on_failure") ActionOnFailure.of_json
    ; status = Util.option_map (Json.lookup j "status") StepStatus.of_json
    }
end

module StepIdsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module Cluster = struct
  type t =
    { id : String.t
    ; name : String.t
    ; status : ClusterStatus.t
    ; ec2_instance_attributes : Ec2InstanceAttributes.t option
    ; instance_collection_type : InstanceCollectionType.t option
    ; log_uri : String.t option
    ; requested_ami_version : String.t option
    ; running_ami_version : String.t option
    ; release_label : String.t option
    ; auto_terminate : Boolean.t option
    ; termination_protected : Boolean.t option
    ; visible_to_all_users : Boolean.t option
    ; applications : ApplicationList.t
    ; tags : TagList.t
    ; service_role : String.t option
    ; normalized_instance_hours : Integer.t option
    ; master_public_dns_name : String.t option
    ; configurations : ConfigurationList.t
    ; security_configuration : String.t option
    ; auto_scaling_role : String.t option
    ; scale_down_behavior : ScaleDownBehavior.t option
    ; custom_ami_id : String.t option
    ; ebs_root_volume_size : Integer.t option
    ; repo_upgrade_on_boot : RepoUpgradeOnBoot.t option
    ; kerberos_attributes : KerberosAttributes.t option
    }

  let make
      ~id
      ~name
      ~status
      ?ec2_instance_attributes
      ?instance_collection_type
      ?log_uri
      ?requested_ami_version
      ?running_ami_version
      ?release_label
      ?auto_terminate
      ?termination_protected
      ?visible_to_all_users
      ?(applications = [])
      ?(tags = [])
      ?service_role
      ?normalized_instance_hours
      ?master_public_dns_name
      ?(configurations = [])
      ?security_configuration
      ?auto_scaling_role
      ?scale_down_behavior
      ?custom_ami_id
      ?ebs_root_volume_size
      ?repo_upgrade_on_boot
      ?kerberos_attributes
      () =
    { id
    ; name
    ; status
    ; ec2_instance_attributes
    ; instance_collection_type
    ; log_uri
    ; requested_ami_version
    ; running_ami_version
    ; release_label
    ; auto_terminate
    ; termination_protected
    ; visible_to_all_users
    ; applications
    ; tags
    ; service_role
    ; normalized_instance_hours
    ; master_public_dns_name
    ; configurations
    ; security_configuration
    ; auto_scaling_role
    ; scale_down_behavior
    ; custom_ami_id
    ; ebs_root_volume_size
    ; repo_upgrade_on_boot
    ; kerberos_attributes
    }

  let parse xml =
    Some
      { id = Xml.required "Id" (Util.option_bind (Xml.member "Id" xml) String.parse)
      ; name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; status =
          Xml.required
            "Status"
            (Util.option_bind (Xml.member "Status" xml) ClusterStatus.parse)
      ; ec2_instance_attributes =
          Util.option_bind
            (Xml.member "Ec2InstanceAttributes" xml)
            Ec2InstanceAttributes.parse
      ; instance_collection_type =
          Util.option_bind
            (Xml.member "InstanceCollectionType" xml)
            InstanceCollectionType.parse
      ; log_uri = Util.option_bind (Xml.member "LogUri" xml) String.parse
      ; requested_ami_version =
          Util.option_bind (Xml.member "RequestedAmiVersion" xml) String.parse
      ; running_ami_version =
          Util.option_bind (Xml.member "RunningAmiVersion" xml) String.parse
      ; release_label = Util.option_bind (Xml.member "ReleaseLabel" xml) String.parse
      ; auto_terminate = Util.option_bind (Xml.member "AutoTerminate" xml) Boolean.parse
      ; termination_protected =
          Util.option_bind (Xml.member "TerminationProtected" xml) Boolean.parse
      ; visible_to_all_users =
          Util.option_bind (Xml.member "VisibleToAllUsers" xml) Boolean.parse
      ; applications =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Applications" xml) ApplicationList.parse)
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; service_role = Util.option_bind (Xml.member "ServiceRole" xml) String.parse
      ; normalized_instance_hours =
          Util.option_bind (Xml.member "NormalizedInstanceHours" xml) Integer.parse
      ; master_public_dns_name =
          Util.option_bind (Xml.member "MasterPublicDnsName" xml) String.parse
      ; configurations =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Configurations" xml) ConfigurationList.parse)
      ; security_configuration =
          Util.option_bind (Xml.member "SecurityConfiguration" xml) String.parse
      ; auto_scaling_role =
          Util.option_bind (Xml.member "AutoScalingRole" xml) String.parse
      ; scale_down_behavior =
          Util.option_bind (Xml.member "ScaleDownBehavior" xml) ScaleDownBehavior.parse
      ; custom_ami_id = Util.option_bind (Xml.member "CustomAmiId" xml) String.parse
      ; ebs_root_volume_size =
          Util.option_bind (Xml.member "EbsRootVolumeSize" xml) Integer.parse
      ; repo_upgrade_on_boot =
          Util.option_bind (Xml.member "RepoUpgradeOnBoot" xml) RepoUpgradeOnBoot.parse
      ; kerberos_attributes =
          Util.option_bind (Xml.member "KerberosAttributes" xml) KerberosAttributes.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.kerberos_attributes (fun f ->
               Query.Pair ("KerberosAttributes", KerberosAttributes.to_query f))
         ; Util.option_map v.repo_upgrade_on_boot (fun f ->
               Query.Pair ("RepoUpgradeOnBoot", RepoUpgradeOnBoot.to_query f))
         ; Util.option_map v.ebs_root_volume_size (fun f ->
               Query.Pair ("EbsRootVolumeSize", Integer.to_query f))
         ; Util.option_map v.custom_ami_id (fun f ->
               Query.Pair ("CustomAmiId", String.to_query f))
         ; Util.option_map v.scale_down_behavior (fun f ->
               Query.Pair ("ScaleDownBehavior", ScaleDownBehavior.to_query f))
         ; Util.option_map v.auto_scaling_role (fun f ->
               Query.Pair ("AutoScalingRole", String.to_query f))
         ; Util.option_map v.security_configuration (fun f ->
               Query.Pair ("SecurityConfiguration", String.to_query f))
         ; Some
             (Query.Pair
                ("Configurations.member", ConfigurationList.to_query v.configurations))
         ; Util.option_map v.master_public_dns_name (fun f ->
               Query.Pair ("MasterPublicDnsName", String.to_query f))
         ; Util.option_map v.normalized_instance_hours (fun f ->
               Query.Pair ("NormalizedInstanceHours", Integer.to_query f))
         ; Util.option_map v.service_role (fun f ->
               Query.Pair ("ServiceRole", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Query.Pair ("Applications.member", ApplicationList.to_query v.applications))
         ; Util.option_map v.visible_to_all_users (fun f ->
               Query.Pair ("VisibleToAllUsers", Boolean.to_query f))
         ; Util.option_map v.termination_protected (fun f ->
               Query.Pair ("TerminationProtected", Boolean.to_query f))
         ; Util.option_map v.auto_terminate (fun f ->
               Query.Pair ("AutoTerminate", Boolean.to_query f))
         ; Util.option_map v.release_label (fun f ->
               Query.Pair ("ReleaseLabel", String.to_query f))
         ; Util.option_map v.running_ami_version (fun f ->
               Query.Pair ("RunningAmiVersion", String.to_query f))
         ; Util.option_map v.requested_ami_version (fun f ->
               Query.Pair ("RequestedAmiVersion", String.to_query f))
         ; Util.option_map v.log_uri (fun f -> Query.Pair ("LogUri", String.to_query f))
         ; Util.option_map v.instance_collection_type (fun f ->
               Query.Pair ("InstanceCollectionType", InstanceCollectionType.to_query f))
         ; Util.option_map v.ec2_instance_attributes (fun f ->
               Query.Pair ("Ec2InstanceAttributes", Ec2InstanceAttributes.to_query f))
         ; Some (Query.Pair ("Status", ClusterStatus.to_query v.status))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ; Some (Query.Pair ("Id", String.to_query v.id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.kerberos_attributes (fun f ->
               "kerberos_attributes", KerberosAttributes.to_json f)
         ; Util.option_map v.repo_upgrade_on_boot (fun f ->
               "repo_upgrade_on_boot", RepoUpgradeOnBoot.to_json f)
         ; Util.option_map v.ebs_root_volume_size (fun f ->
               "ebs_root_volume_size", Integer.to_json f)
         ; Util.option_map v.custom_ami_id (fun f -> "custom_ami_id", String.to_json f)
         ; Util.option_map v.scale_down_behavior (fun f ->
               "scale_down_behavior", ScaleDownBehavior.to_json f)
         ; Util.option_map v.auto_scaling_role (fun f ->
               "auto_scaling_role", String.to_json f)
         ; Util.option_map v.security_configuration (fun f ->
               "security_configuration", String.to_json f)
         ; Some ("configurations", ConfigurationList.to_json v.configurations)
         ; Util.option_map v.master_public_dns_name (fun f ->
               "master_public_dns_name", String.to_json f)
         ; Util.option_map v.normalized_instance_hours (fun f ->
               "normalized_instance_hours", Integer.to_json f)
         ; Util.option_map v.service_role (fun f -> "service_role", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Some ("applications", ApplicationList.to_json v.applications)
         ; Util.option_map v.visible_to_all_users (fun f ->
               "visible_to_all_users", Boolean.to_json f)
         ; Util.option_map v.termination_protected (fun f ->
               "termination_protected", Boolean.to_json f)
         ; Util.option_map v.auto_terminate (fun f -> "auto_terminate", Boolean.to_json f)
         ; Util.option_map v.release_label (fun f -> "release_label", String.to_json f)
         ; Util.option_map v.running_ami_version (fun f ->
               "running_ami_version", String.to_json f)
         ; Util.option_map v.requested_ami_version (fun f ->
               "requested_ami_version", String.to_json f)
         ; Util.option_map v.log_uri (fun f -> "log_uri", String.to_json f)
         ; Util.option_map v.instance_collection_type (fun f ->
               "instance_collection_type", InstanceCollectionType.to_json f)
         ; Util.option_map v.ec2_instance_attributes (fun f ->
               "ec2_instance_attributes", Ec2InstanceAttributes.to_json f)
         ; Some ("status", ClusterStatus.to_json v.status)
         ; Some ("name", String.to_json v.name)
         ; Some ("id", String.to_json v.id)
         ])

  let of_json j =
    { id = String.of_json (Util.of_option_exn (Json.lookup j "id"))
    ; name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; status = ClusterStatus.of_json (Util.of_option_exn (Json.lookup j "status"))
    ; ec2_instance_attributes =
        Util.option_map
          (Json.lookup j "ec2_instance_attributes")
          Ec2InstanceAttributes.of_json
    ; instance_collection_type =
        Util.option_map
          (Json.lookup j "instance_collection_type")
          InstanceCollectionType.of_json
    ; log_uri = Util.option_map (Json.lookup j "log_uri") String.of_json
    ; requested_ami_version =
        Util.option_map (Json.lookup j "requested_ami_version") String.of_json
    ; running_ami_version =
        Util.option_map (Json.lookup j "running_ami_version") String.of_json
    ; release_label = Util.option_map (Json.lookup j "release_label") String.of_json
    ; auto_terminate = Util.option_map (Json.lookup j "auto_terminate") Boolean.of_json
    ; termination_protected =
        Util.option_map (Json.lookup j "termination_protected") Boolean.of_json
    ; visible_to_all_users =
        Util.option_map (Json.lookup j "visible_to_all_users") Boolean.of_json
    ; applications =
        ApplicationList.of_json (Util.of_option_exn (Json.lookup j "applications"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; service_role = Util.option_map (Json.lookup j "service_role") String.of_json
    ; normalized_instance_hours =
        Util.option_map (Json.lookup j "normalized_instance_hours") Integer.of_json
    ; master_public_dns_name =
        Util.option_map (Json.lookup j "master_public_dns_name") String.of_json
    ; configurations =
        ConfigurationList.of_json (Util.of_option_exn (Json.lookup j "configurations"))
    ; security_configuration =
        Util.option_map (Json.lookup j "security_configuration") String.of_json
    ; auto_scaling_role =
        Util.option_map (Json.lookup j "auto_scaling_role") String.of_json
    ; scale_down_behavior =
        Util.option_map (Json.lookup j "scale_down_behavior") ScaleDownBehavior.of_json
    ; custom_ami_id = Util.option_map (Json.lookup j "custom_ami_id") String.of_json
    ; ebs_root_volume_size =
        Util.option_map (Json.lookup j "ebs_root_volume_size") Integer.of_json
    ; repo_upgrade_on_boot =
        Util.option_map (Json.lookup j "repo_upgrade_on_boot") RepoUpgradeOnBoot.of_json
    ; kerberos_attributes =
        Util.option_map (Json.lookup j "kerberos_attributes") KerberosAttributes.of_json
    }
end

module BootstrapActionConfigList = struct
  type t = BootstrapActionConfig.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map BootstrapActionConfig.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list BootstrapActionConfig.to_query v

  let to_json v = `List (List.map BootstrapActionConfig.to_json v)

  let of_json j = Json.to_list BootstrapActionConfig.of_json j
end

module JobFlowInstancesConfig = struct
  type t =
    { master_instance_type : String.t option
    ; slave_instance_type : String.t option
    ; instance_count : Integer.t option
    ; instance_groups : InstanceGroupConfigList.t
    ; instance_fleets : InstanceFleetConfigList.t
    ; ec2_key_name : String.t option
    ; placement : PlacementType.t option
    ; keep_job_flow_alive_when_no_steps : Boolean.t option
    ; termination_protected : Boolean.t option
    ; hadoop_version : String.t option
    ; ec2_subnet_id : String.t option
    ; ec2_subnet_ids : XmlStringMaxLen256List.t
    ; emr_managed_master_security_group : String.t option
    ; emr_managed_slave_security_group : String.t option
    ; service_access_security_group : String.t option
    ; additional_master_security_groups : SecurityGroupsList.t
    ; additional_slave_security_groups : SecurityGroupsList.t
    }

  let make
      ?master_instance_type
      ?slave_instance_type
      ?instance_count
      ?(instance_groups = [])
      ?(instance_fleets = [])
      ?ec2_key_name
      ?placement
      ?keep_job_flow_alive_when_no_steps
      ?termination_protected
      ?hadoop_version
      ?ec2_subnet_id
      ?(ec2_subnet_ids = [])
      ?emr_managed_master_security_group
      ?emr_managed_slave_security_group
      ?service_access_security_group
      ?(additional_master_security_groups = [])
      ?(additional_slave_security_groups = [])
      () =
    { master_instance_type
    ; slave_instance_type
    ; instance_count
    ; instance_groups
    ; instance_fleets
    ; ec2_key_name
    ; placement
    ; keep_job_flow_alive_when_no_steps
    ; termination_protected
    ; hadoop_version
    ; ec2_subnet_id
    ; ec2_subnet_ids
    ; emr_managed_master_security_group
    ; emr_managed_slave_security_group
    ; service_access_security_group
    ; additional_master_security_groups
    ; additional_slave_security_groups
    }

  let parse xml =
    Some
      { master_instance_type =
          Util.option_bind (Xml.member "MasterInstanceType" xml) String.parse
      ; slave_instance_type =
          Util.option_bind (Xml.member "SlaveInstanceType" xml) String.parse
      ; instance_count = Util.option_bind (Xml.member "InstanceCount" xml) Integer.parse
      ; instance_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstanceGroups" xml)
               InstanceGroupConfigList.parse)
      ; instance_fleets =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstanceFleets" xml)
               InstanceFleetConfigList.parse)
      ; ec2_key_name = Util.option_bind (Xml.member "Ec2KeyName" xml) String.parse
      ; placement = Util.option_bind (Xml.member "Placement" xml) PlacementType.parse
      ; keep_job_flow_alive_when_no_steps =
          Util.option_bind (Xml.member "KeepJobFlowAliveWhenNoSteps" xml) Boolean.parse
      ; termination_protected =
          Util.option_bind (Xml.member "TerminationProtected" xml) Boolean.parse
      ; hadoop_version = Util.option_bind (Xml.member "HadoopVersion" xml) String.parse
      ; ec2_subnet_id = Util.option_bind (Xml.member "Ec2SubnetId" xml) String.parse
      ; ec2_subnet_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "Ec2SubnetIds" xml)
               XmlStringMaxLen256List.parse)
      ; emr_managed_master_security_group =
          Util.option_bind (Xml.member "EmrManagedMasterSecurityGroup" xml) String.parse
      ; emr_managed_slave_security_group =
          Util.option_bind (Xml.member "EmrManagedSlaveSecurityGroup" xml) String.parse
      ; service_access_security_group =
          Util.option_bind (Xml.member "ServiceAccessSecurityGroup" xml) String.parse
      ; additional_master_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AdditionalMasterSecurityGroups" xml)
               SecurityGroupsList.parse)
      ; additional_slave_security_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "AdditionalSlaveSecurityGroups" xml)
               SecurityGroupsList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "AdditionalSlaveSecurityGroups.member"
                , SecurityGroupsList.to_query v.additional_slave_security_groups ))
         ; Some
             (Query.Pair
                ( "AdditionalMasterSecurityGroups.member"
                , SecurityGroupsList.to_query v.additional_master_security_groups ))
         ; Util.option_map v.service_access_security_group (fun f ->
               Query.Pair ("ServiceAccessSecurityGroup", String.to_query f))
         ; Util.option_map v.emr_managed_slave_security_group (fun f ->
               Query.Pair ("EmrManagedSlaveSecurityGroup", String.to_query f))
         ; Util.option_map v.emr_managed_master_security_group (fun f ->
               Query.Pair ("EmrManagedMasterSecurityGroup", String.to_query f))
         ; Some
             (Query.Pair
                ("Ec2SubnetIds.member", XmlStringMaxLen256List.to_query v.ec2_subnet_ids))
         ; Util.option_map v.ec2_subnet_id (fun f ->
               Query.Pair ("Ec2SubnetId", String.to_query f))
         ; Util.option_map v.hadoop_version (fun f ->
               Query.Pair ("HadoopVersion", String.to_query f))
         ; Util.option_map v.termination_protected (fun f ->
               Query.Pair ("TerminationProtected", Boolean.to_query f))
         ; Util.option_map v.keep_job_flow_alive_when_no_steps (fun f ->
               Query.Pair ("KeepJobFlowAliveWhenNoSteps", Boolean.to_query f))
         ; Util.option_map v.placement (fun f ->
               Query.Pair ("Placement", PlacementType.to_query f))
         ; Util.option_map v.ec2_key_name (fun f ->
               Query.Pair ("Ec2KeyName", String.to_query f))
         ; Some
             (Query.Pair
                ( "InstanceFleets.member"
                , InstanceFleetConfigList.to_query v.instance_fleets ))
         ; Some
             (Query.Pair
                ( "InstanceGroups.member"
                , InstanceGroupConfigList.to_query v.instance_groups ))
         ; Util.option_map v.instance_count (fun f ->
               Query.Pair ("InstanceCount", Integer.to_query f))
         ; Util.option_map v.slave_instance_type (fun f ->
               Query.Pair ("SlaveInstanceType", String.to_query f))
         ; Util.option_map v.master_instance_type (fun f ->
               Query.Pair ("MasterInstanceType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "additional_slave_security_groups"
             , SecurityGroupsList.to_json v.additional_slave_security_groups )
         ; Some
             ( "additional_master_security_groups"
             , SecurityGroupsList.to_json v.additional_master_security_groups )
         ; Util.option_map v.service_access_security_group (fun f ->
               "service_access_security_group", String.to_json f)
         ; Util.option_map v.emr_managed_slave_security_group (fun f ->
               "emr_managed_slave_security_group", String.to_json f)
         ; Util.option_map v.emr_managed_master_security_group (fun f ->
               "emr_managed_master_security_group", String.to_json f)
         ; Some ("ec2_subnet_ids", XmlStringMaxLen256List.to_json v.ec2_subnet_ids)
         ; Util.option_map v.ec2_subnet_id (fun f -> "ec2_subnet_id", String.to_json f)
         ; Util.option_map v.hadoop_version (fun f -> "hadoop_version", String.to_json f)
         ; Util.option_map v.termination_protected (fun f ->
               "termination_protected", Boolean.to_json f)
         ; Util.option_map v.keep_job_flow_alive_when_no_steps (fun f ->
               "keep_job_flow_alive_when_no_steps", Boolean.to_json f)
         ; Util.option_map v.placement (fun f -> "placement", PlacementType.to_json f)
         ; Util.option_map v.ec2_key_name (fun f -> "ec2_key_name", String.to_json f)
         ; Some ("instance_fleets", InstanceFleetConfigList.to_json v.instance_fleets)
         ; Some ("instance_groups", InstanceGroupConfigList.to_json v.instance_groups)
         ; Util.option_map v.instance_count (fun f -> "instance_count", Integer.to_json f)
         ; Util.option_map v.slave_instance_type (fun f ->
               "slave_instance_type", String.to_json f)
         ; Util.option_map v.master_instance_type (fun f ->
               "master_instance_type", String.to_json f)
         ])

  let of_json j =
    { master_instance_type =
        Util.option_map (Json.lookup j "master_instance_type") String.of_json
    ; slave_instance_type =
        Util.option_map (Json.lookup j "slave_instance_type") String.of_json
    ; instance_count = Util.option_map (Json.lookup j "instance_count") Integer.of_json
    ; instance_groups =
        InstanceGroupConfigList.of_json
          (Util.of_option_exn (Json.lookup j "instance_groups"))
    ; instance_fleets =
        InstanceFleetConfigList.of_json
          (Util.of_option_exn (Json.lookup j "instance_fleets"))
    ; ec2_key_name = Util.option_map (Json.lookup j "ec2_key_name") String.of_json
    ; placement = Util.option_map (Json.lookup j "placement") PlacementType.of_json
    ; keep_job_flow_alive_when_no_steps =
        Util.option_map
          (Json.lookup j "keep_job_flow_alive_when_no_steps")
          Boolean.of_json
    ; termination_protected =
        Util.option_map (Json.lookup j "termination_protected") Boolean.of_json
    ; hadoop_version = Util.option_map (Json.lookup j "hadoop_version") String.of_json
    ; ec2_subnet_id = Util.option_map (Json.lookup j "ec2_subnet_id") String.of_json
    ; ec2_subnet_ids =
        XmlStringMaxLen256List.of_json
          (Util.of_option_exn (Json.lookup j "ec2_subnet_ids"))
    ; emr_managed_master_security_group =
        Util.option_map (Json.lookup j "emr_managed_master_security_group") String.of_json
    ; emr_managed_slave_security_group =
        Util.option_map (Json.lookup j "emr_managed_slave_security_group") String.of_json
    ; service_access_security_group =
        Util.option_map (Json.lookup j "service_access_security_group") String.of_json
    ; additional_master_security_groups =
        SecurityGroupsList.of_json
          (Util.of_option_exn (Json.lookup j "additional_master_security_groups"))
    ; additional_slave_security_groups =
        SecurityGroupsList.of_json
          (Util.of_option_exn (Json.lookup j "additional_slave_security_groups"))
    }
end

module NewSupportedProductsList = struct
  type t = SupportedProductConfig.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map SupportedProductConfig.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list SupportedProductConfig.to_query v

  let to_json v = `List (List.map SupportedProductConfig.to_json v)

  let of_json j = Json.to_list SupportedProductConfig.of_json j
end

module StepConfigList = struct
  type t = StepConfig.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map StepConfig.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list StepConfig.to_query v

  let to_json v = `List (List.map StepConfig.to_json v)

  let of_json j = Json.to_list StepConfig.of_json j
end

module SecurityConfigurationList = struct
  type t = SecurityConfigurationSummary.t list

  let make elems () = elems

  let parse xml =
    Util.option_all
      (List.map SecurityConfigurationSummary.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list SecurityConfigurationSummary.to_query v

  let to_json v = `List (List.map SecurityConfigurationSummary.to_json v)

  let of_json j = Json.to_list SecurityConfigurationSummary.of_json j
end

module InstanceFleetList = struct
  type t = InstanceFleet.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceFleet.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceFleet.to_query v

  let to_json v = `List (List.map InstanceFleet.to_json v)

  let of_json j = Json.to_list InstanceFleet.of_json j
end

module StepSummaryList = struct
  type t = StepSummary.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map StepSummary.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list StepSummary.to_query v

  let to_json v = `List (List.map StepSummary.to_json v)

  let of_json j = Json.to_list StepSummary.of_json j
end

module InstanceGroupModifyConfigList = struct
  type t = InstanceGroupModifyConfig.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceGroupModifyConfig.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceGroupModifyConfig.to_query v

  let to_json v = `List (List.map InstanceGroupModifyConfig.to_json v)

  let of_json j = Json.to_list InstanceGroupModifyConfig.of_json j
end

module InstanceGroupList = struct
  type t = InstanceGroup.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceGroup.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceGroup.to_query v

  let to_json v = `List (List.map InstanceGroup.to_json v)

  let of_json j = Json.to_list InstanceGroup.of_json j
end

module InstanceList = struct
  type t = Instance.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Instance.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Instance.to_query v

  let to_json v = `List (List.map Instance.to_json v)

  let of_json j = Json.to_list Instance.of_json j
end

module ClusterSummaryList = struct
  type t = ClusterSummary.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map ClusterSummary.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list ClusterSummary.to_query v

  let to_json v = `List (List.map ClusterSummary.to_json v)

  let of_json j = Json.to_list ClusterSummary.of_json j
end

module StepStateList = struct
  type t = StepState.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map StepState.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list StepState.to_query v

  let to_json v = `List (List.map StepState.to_json v)

  let of_json j = Json.to_list StepState.of_json j
end

module JobFlowExecutionStateList = struct
  type t = JobFlowExecutionState.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map JobFlowExecutionState.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list JobFlowExecutionState.to_query v

  let to_json v = `List (List.map JobFlowExecutionState.to_json v)

  let of_json j = Json.to_list JobFlowExecutionState.of_json j
end

module JobFlowDetailList = struct
  type t = JobFlowDetail.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map JobFlowDetail.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list JobFlowDetail.to_query v

  let to_json v = `List (List.map JobFlowDetail.to_json v)

  let of_json j = Json.to_list JobFlowDetail.of_json j
end

module CommandList = struct
  type t = Command.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map Command.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list Command.to_query v

  let to_json v = `List (List.map Command.to_json v)

  let of_json j = Json.to_list Command.of_json j
end

module InstanceGroupIdsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml = Util.option_all (List.map String.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Json.to_list String.of_json j
end

module CancelStepsInfoList = struct
  type t = CancelStepsInfo.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map CancelStepsInfo.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list CancelStepsInfo.to_query v

  let to_json v = `List (List.map CancelStepsInfo.to_json v)

  let of_json j = Json.to_list CancelStepsInfo.of_json j
end

module InstanceGroupTypeList = struct
  type t = InstanceGroupType.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceGroupType.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceGroupType.to_query v

  let to_json v = `List (List.map InstanceGroupType.to_json v)

  let of_json j = Json.to_list InstanceGroupType.of_json j
end

module InstanceStateList = struct
  type t = InstanceState.t list

  let make elems () = elems

  let parse xml =
    Util.option_all (List.map InstanceState.parse (Xml.members "member" xml))

  let to_query v = Query.to_query_list InstanceState.to_query v

  let to_json v = `List (List.map InstanceState.to_json v)

  let of_json j = Json.to_list InstanceState.of_json j
end

module ListInstanceGroupsInput = struct
  type t =
    { cluster_id : String.t
    ; marker : String.t option
    }

  let make ~cluster_id ?marker () = { cluster_id; marker }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module AddTagsInput = struct
  type t =
    { resource_id : String.t
    ; tags : TagList.t
    }

  let make ~resource_id ~tags () = { resource_id; tags }

  let parse xml =
    Some
      { resource_id =
          Xml.required
            "ResourceId"
            (Util.option_bind (Xml.member "ResourceId" xml) String.parse)
      ; tags =
          Xml.required "Tags" (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Query.Pair ("ResourceId", String.to_query v.resource_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tags", TagList.to_json v.tags)
         ; Some ("resource_id", String.to_json v.resource_id)
         ])

  let of_json j =
    { resource_id = String.of_json (Util.of_option_exn (Json.lookup j "resource_id"))
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    }
end

module ListClustersInput = struct
  type t =
    { created_after : DateTime.t option
    ; created_before : DateTime.t option
    ; cluster_states : ClusterStateList.t
    ; marker : String.t option
    }

  let make ?created_after ?created_before ?(cluster_states = []) ?marker () =
    { created_after; created_before; cluster_states; marker }

  let parse xml =
    Some
      { created_after = Util.option_bind (Xml.member "CreatedAfter" xml) DateTime.parse
      ; created_before = Util.option_bind (Xml.member "CreatedBefore" xml) DateTime.parse
      ; cluster_states =
          Util.of_option
            []
            (Util.option_bind (Xml.member "ClusterStates" xml) ClusterStateList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ("ClusterStates.member", ClusterStateList.to_query v.cluster_states))
         ; Util.option_map v.created_before (fun f ->
               Query.Pair ("CreatedBefore", DateTime.to_query f))
         ; Util.option_map v.created_after (fun f ->
               Query.Pair ("CreatedAfter", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("cluster_states", ClusterStateList.to_json v.cluster_states)
         ; Util.option_map v.created_before (fun f ->
               "created_before", DateTime.to_json f)
         ; Util.option_map v.created_after (fun f -> "created_after", DateTime.to_json f)
         ])

  let of_json j =
    { created_after = Util.option_map (Json.lookup j "created_after") DateTime.of_json
    ; created_before = Util.option_map (Json.lookup j "created_before") DateTime.of_json
    ; cluster_states =
        ClusterStateList.of_json (Util.of_option_exn (Json.lookup j "cluster_states"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DeleteSecurityConfigurationInput = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v = `Assoc (Util.list_filter_opt [ Some ("name", String.to_json v.name) ])

  let of_json j = { name = String.of_json (Util.of_option_exn (Json.lookup j "name")) }
end

module ModifyInstanceFleetInput = struct
  type t =
    { cluster_id : String.t
    ; instance_fleet : InstanceFleetModifyConfig.t
    }

  let make ~cluster_id ~instance_fleet () = { cluster_id; instance_fleet }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; instance_fleet =
          Xml.required
            "InstanceFleet"
            (Util.option_bind
               (Xml.member "InstanceFleet" xml)
               InstanceFleetModifyConfig.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("InstanceFleet", InstanceFleetModifyConfig.to_query v.instance_fleet))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("instance_fleet", InstanceFleetModifyConfig.to_json v.instance_fleet)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; instance_fleet =
        InstanceFleetModifyConfig.of_json
          (Util.of_option_exn (Json.lookup j "instance_fleet"))
    }
end

module DescribeStepOutput = struct
  type t = { step : Step.t option }

  let make ?step () = { step }

  let parse xml = Some { step = Util.option_bind (Xml.member "Step" xml) Step.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.step (fun f -> Query.Pair ("Step", Step.to_query f)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Util.option_map v.step (fun f -> "step", Step.to_json f) ])

  let of_json j = { step = Util.option_map (Json.lookup j "step") Step.of_json }
end

module AddInstanceGroupsInput = struct
  type t =
    { instance_groups : InstanceGroupConfigList.t
    ; job_flow_id : String.t
    }

  let make ~instance_groups ~job_flow_id () = { instance_groups; job_flow_id }

  let parse xml =
    Some
      { instance_groups =
          Xml.required
            "InstanceGroups"
            (Util.option_bind
               (Xml.member "InstanceGroups" xml)
               InstanceGroupConfigList.parse)
      ; job_flow_id =
          Xml.required
            "JobFlowId"
            (Util.option_bind (Xml.member "JobFlowId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("JobFlowId", String.to_query v.job_flow_id))
         ; Some
             (Query.Pair
                ( "InstanceGroups.member"
                , InstanceGroupConfigList.to_query v.instance_groups ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("job_flow_id", String.to_json v.job_flow_id)
         ; Some ("instance_groups", InstanceGroupConfigList.to_json v.instance_groups)
         ])

  let of_json j =
    { instance_groups =
        InstanceGroupConfigList.of_json
          (Util.of_option_exn (Json.lookup j "instance_groups"))
    ; job_flow_id = String.of_json (Util.of_option_exn (Json.lookup j "job_flow_id"))
    }
end

module DescribeSecurityConfigurationInput = struct
  type t = { name : String.t }

  let make ~name () = { name }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("Name", String.to_query v.name)) ])

  let to_json v = `Assoc (Util.list_filter_opt [ Some ("name", String.to_json v.name) ])

  let of_json j = { name = String.of_json (Util.of_option_exn (Json.lookup j "name")) }
end

module ListInstanceFleetsInput = struct
  type t =
    { cluster_id : String.t
    ; marker : String.t option
    }

  let make ~cluster_id ?marker () = { cluster_id; marker }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module CancelStepsInput = struct
  type t =
    { cluster_id : String.t option
    ; step_ids : StepIdsList.t
    }

  let make ?cluster_id ?(step_ids = []) () = { cluster_id; step_ids }

  let parse xml =
    Some
      { cluster_id = Util.option_bind (Xml.member "ClusterId" xml) String.parse
      ; step_ids =
          Util.of_option
            []
            (Util.option_bind (Xml.member "StepIds" xml) StepIdsList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("StepIds.member", StepIdsList.to_query v.step_ids))
         ; Util.option_map v.cluster_id (fun f ->
               Query.Pair ("ClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("step_ids", StepIdsList.to_json v.step_ids)
         ; Util.option_map v.cluster_id (fun f -> "cluster_id", String.to_json f)
         ])

  let of_json j =
    { cluster_id = Util.option_map (Json.lookup j "cluster_id") String.of_json
    ; step_ids = StepIdsList.of_json (Util.of_option_exn (Json.lookup j "step_ids"))
    }
end

module SetVisibleToAllUsersInput = struct
  type t =
    { job_flow_ids : XmlStringList.t
    ; visible_to_all_users : Boolean.t
    }

  let make ~job_flow_ids ~visible_to_all_users () = { job_flow_ids; visible_to_all_users }

  let parse xml =
    Some
      { job_flow_ids =
          Xml.required
            "JobFlowIds"
            (Util.option_bind (Xml.member "JobFlowIds" xml) XmlStringList.parse)
      ; visible_to_all_users =
          Xml.required
            "VisibleToAllUsers"
            (Util.option_bind (Xml.member "VisibleToAllUsers" xml) Boolean.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("VisibleToAllUsers", Boolean.to_query v.visible_to_all_users))
         ; Some (Query.Pair ("JobFlowIds.member", XmlStringList.to_query v.job_flow_ids))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("visible_to_all_users", Boolean.to_json v.visible_to_all_users)
         ; Some ("job_flow_ids", XmlStringList.to_json v.job_flow_ids)
         ])

  let of_json j =
    { job_flow_ids =
        XmlStringList.of_json (Util.of_option_exn (Json.lookup j "job_flow_ids"))
    ; visible_to_all_users =
        Boolean.of_json (Util.of_option_exn (Json.lookup j "visible_to_all_users"))
    }
end

module CreateSecurityConfigurationInput = struct
  type t =
    { name : String.t
    ; security_configuration : String.t
    }

  let make ~name ~security_configuration () = { name; security_configuration }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; security_configuration =
          Xml.required
            "SecurityConfiguration"
            (Util.option_bind (Xml.member "SecurityConfiguration" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("SecurityConfiguration", String.to_query v.security_configuration))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("security_configuration", String.to_json v.security_configuration)
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; security_configuration =
        String.of_json (Util.of_option_exn (Json.lookup j "security_configuration"))
    }
end

module ListBootstrapActionsInput = struct
  type t =
    { cluster_id : String.t
    ; marker : String.t option
    }

  let make ~cluster_id ?marker () = { cluster_id; marker }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DescribeClusterOutput = struct
  type t = { cluster : Cluster.t }

  let make ~cluster () = { cluster }

  let parse xml =
    Some
      { cluster =
          Xml.required
            "Cluster"
            (Util.option_bind (Xml.member "Cluster" xml) Cluster.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt [ Some (Query.Pair ("Cluster", Cluster.to_query v.cluster)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("cluster", Cluster.to_json v.cluster) ])

  let of_json j =
    { cluster = Cluster.of_json (Util.of_option_exn (Json.lookup j "cluster")) }
end

module DescribeStepInput = struct
  type t =
    { cluster_id : String.t
    ; step_id : String.t
    }

  let make ~cluster_id ~step_id () = { cluster_id; step_id }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; step_id =
          Xml.required "StepId" (Util.option_bind (Xml.member "StepId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("StepId", String.to_query v.step_id))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("step_id", String.to_json v.step_id)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; step_id = String.of_json (Util.of_option_exn (Json.lookup j "step_id"))
    }
end

module RunJobFlowInput = struct
  type t =
    { name : String.t
    ; log_uri : String.t option
    ; additional_info : String.t option
    ; ami_version : String.t option
    ; release_label : String.t option
    ; instances : JobFlowInstancesConfig.t
    ; steps : StepConfigList.t
    ; bootstrap_actions : BootstrapActionConfigList.t
    ; supported_products : SupportedProductsList.t
    ; new_supported_products : NewSupportedProductsList.t
    ; applications : ApplicationList.t
    ; configurations : ConfigurationList.t
    ; visible_to_all_users : Boolean.t option
    ; job_flow_role : String.t option
    ; service_role : String.t option
    ; tags : TagList.t
    ; security_configuration : String.t option
    ; auto_scaling_role : String.t option
    ; scale_down_behavior : ScaleDownBehavior.t option
    ; custom_ami_id : String.t option
    ; ebs_root_volume_size : Integer.t option
    ; repo_upgrade_on_boot : RepoUpgradeOnBoot.t option
    ; kerberos_attributes : KerberosAttributes.t option
    }

  let make
      ~name
      ?log_uri
      ?additional_info
      ?ami_version
      ?release_label
      ~instances
      ?(steps = [])
      ?(bootstrap_actions = [])
      ?(supported_products = [])
      ?(new_supported_products = [])
      ?(applications = [])
      ?(configurations = [])
      ?visible_to_all_users
      ?job_flow_role
      ?service_role
      ?(tags = [])
      ?security_configuration
      ?auto_scaling_role
      ?scale_down_behavior
      ?custom_ami_id
      ?ebs_root_volume_size
      ?repo_upgrade_on_boot
      ?kerberos_attributes
      () =
    { name
    ; log_uri
    ; additional_info
    ; ami_version
    ; release_label
    ; instances
    ; steps
    ; bootstrap_actions
    ; supported_products
    ; new_supported_products
    ; applications
    ; configurations
    ; visible_to_all_users
    ; job_flow_role
    ; service_role
    ; tags
    ; security_configuration
    ; auto_scaling_role
    ; scale_down_behavior
    ; custom_ami_id
    ; ebs_root_volume_size
    ; repo_upgrade_on_boot
    ; kerberos_attributes
    }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; log_uri = Util.option_bind (Xml.member "LogUri" xml) String.parse
      ; additional_info = Util.option_bind (Xml.member "AdditionalInfo" xml) String.parse
      ; ami_version = Util.option_bind (Xml.member "AmiVersion" xml) String.parse
      ; release_label = Util.option_bind (Xml.member "ReleaseLabel" xml) String.parse
      ; instances =
          Xml.required
            "Instances"
            (Util.option_bind (Xml.member "Instances" xml) JobFlowInstancesConfig.parse)
      ; steps =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Steps" xml) StepConfigList.parse)
      ; bootstrap_actions =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "BootstrapActions" xml)
               BootstrapActionConfigList.parse)
      ; supported_products =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SupportedProducts" xml)
               SupportedProductsList.parse)
      ; new_supported_products =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "NewSupportedProducts" xml)
               NewSupportedProductsList.parse)
      ; applications =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Applications" xml) ApplicationList.parse)
      ; configurations =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Configurations" xml) ConfigurationList.parse)
      ; visible_to_all_users =
          Util.option_bind (Xml.member "VisibleToAllUsers" xml) Boolean.parse
      ; job_flow_role = Util.option_bind (Xml.member "JobFlowRole" xml) String.parse
      ; service_role = Util.option_bind (Xml.member "ServiceRole" xml) String.parse
      ; tags = Util.of_option [] (Util.option_bind (Xml.member "Tags" xml) TagList.parse)
      ; security_configuration =
          Util.option_bind (Xml.member "SecurityConfiguration" xml) String.parse
      ; auto_scaling_role =
          Util.option_bind (Xml.member "AutoScalingRole" xml) String.parse
      ; scale_down_behavior =
          Util.option_bind (Xml.member "ScaleDownBehavior" xml) ScaleDownBehavior.parse
      ; custom_ami_id = Util.option_bind (Xml.member "CustomAmiId" xml) String.parse
      ; ebs_root_volume_size =
          Util.option_bind (Xml.member "EbsRootVolumeSize" xml) Integer.parse
      ; repo_upgrade_on_boot =
          Util.option_bind (Xml.member "RepoUpgradeOnBoot" xml) RepoUpgradeOnBoot.parse
      ; kerberos_attributes =
          Util.option_bind (Xml.member "KerberosAttributes" xml) KerberosAttributes.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.kerberos_attributes (fun f ->
               Query.Pair ("KerberosAttributes", KerberosAttributes.to_query f))
         ; Util.option_map v.repo_upgrade_on_boot (fun f ->
               Query.Pair ("RepoUpgradeOnBoot", RepoUpgradeOnBoot.to_query f))
         ; Util.option_map v.ebs_root_volume_size (fun f ->
               Query.Pair ("EbsRootVolumeSize", Integer.to_query f))
         ; Util.option_map v.custom_ami_id (fun f ->
               Query.Pair ("CustomAmiId", String.to_query f))
         ; Util.option_map v.scale_down_behavior (fun f ->
               Query.Pair ("ScaleDownBehavior", ScaleDownBehavior.to_query f))
         ; Util.option_map v.auto_scaling_role (fun f ->
               Query.Pair ("AutoScalingRole", String.to_query f))
         ; Util.option_map v.security_configuration (fun f ->
               Query.Pair ("SecurityConfiguration", String.to_query f))
         ; Some (Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Util.option_map v.service_role (fun f ->
               Query.Pair ("ServiceRole", String.to_query f))
         ; Util.option_map v.job_flow_role (fun f ->
               Query.Pair ("JobFlowRole", String.to_query f))
         ; Util.option_map v.visible_to_all_users (fun f ->
               Query.Pair ("VisibleToAllUsers", Boolean.to_query f))
         ; Some
             (Query.Pair
                ("Configurations.member", ConfigurationList.to_query v.configurations))
         ; Some
             (Query.Pair ("Applications.member", ApplicationList.to_query v.applications))
         ; Some
             (Query.Pair
                ( "NewSupportedProducts.member"
                , NewSupportedProductsList.to_query v.new_supported_products ))
         ; Some
             (Query.Pair
                ( "SupportedProducts.member"
                , SupportedProductsList.to_query v.supported_products ))
         ; Some
             (Query.Pair
                ( "BootstrapActions.member"
                , BootstrapActionConfigList.to_query v.bootstrap_actions ))
         ; Some (Query.Pair ("Steps.member", StepConfigList.to_query v.steps))
         ; Some (Query.Pair ("Instances", JobFlowInstancesConfig.to_query v.instances))
         ; Util.option_map v.release_label (fun f ->
               Query.Pair ("ReleaseLabel", String.to_query f))
         ; Util.option_map v.ami_version (fun f ->
               Query.Pair ("AmiVersion", String.to_query f))
         ; Util.option_map v.additional_info (fun f ->
               Query.Pair ("AdditionalInfo", String.to_query f))
         ; Util.option_map v.log_uri (fun f -> Query.Pair ("LogUri", String.to_query f))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.kerberos_attributes (fun f ->
               "kerberos_attributes", KerberosAttributes.to_json f)
         ; Util.option_map v.repo_upgrade_on_boot (fun f ->
               "repo_upgrade_on_boot", RepoUpgradeOnBoot.to_json f)
         ; Util.option_map v.ebs_root_volume_size (fun f ->
               "ebs_root_volume_size", Integer.to_json f)
         ; Util.option_map v.custom_ami_id (fun f -> "custom_ami_id", String.to_json f)
         ; Util.option_map v.scale_down_behavior (fun f ->
               "scale_down_behavior", ScaleDownBehavior.to_json f)
         ; Util.option_map v.auto_scaling_role (fun f ->
               "auto_scaling_role", String.to_json f)
         ; Util.option_map v.security_configuration (fun f ->
               "security_configuration", String.to_json f)
         ; Some ("tags", TagList.to_json v.tags)
         ; Util.option_map v.service_role (fun f -> "service_role", String.to_json f)
         ; Util.option_map v.job_flow_role (fun f -> "job_flow_role", String.to_json f)
         ; Util.option_map v.visible_to_all_users (fun f ->
               "visible_to_all_users", Boolean.to_json f)
         ; Some ("configurations", ConfigurationList.to_json v.configurations)
         ; Some ("applications", ApplicationList.to_json v.applications)
         ; Some
             ( "new_supported_products"
             , NewSupportedProductsList.to_json v.new_supported_products )
         ; Some ("supported_products", SupportedProductsList.to_json v.supported_products)
         ; Some
             ("bootstrap_actions", BootstrapActionConfigList.to_json v.bootstrap_actions)
         ; Some ("steps", StepConfigList.to_json v.steps)
         ; Some ("instances", JobFlowInstancesConfig.to_json v.instances)
         ; Util.option_map v.release_label (fun f -> "release_label", String.to_json f)
         ; Util.option_map v.ami_version (fun f -> "ami_version", String.to_json f)
         ; Util.option_map v.additional_info (fun f ->
               "additional_info", String.to_json f)
         ; Util.option_map v.log_uri (fun f -> "log_uri", String.to_json f)
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; log_uri = Util.option_map (Json.lookup j "log_uri") String.of_json
    ; additional_info = Util.option_map (Json.lookup j "additional_info") String.of_json
    ; ami_version = Util.option_map (Json.lookup j "ami_version") String.of_json
    ; release_label = Util.option_map (Json.lookup j "release_label") String.of_json
    ; instances =
        JobFlowInstancesConfig.of_json (Util.of_option_exn (Json.lookup j "instances"))
    ; steps = StepConfigList.of_json (Util.of_option_exn (Json.lookup j "steps"))
    ; bootstrap_actions =
        BootstrapActionConfigList.of_json
          (Util.of_option_exn (Json.lookup j "bootstrap_actions"))
    ; supported_products =
        SupportedProductsList.of_json
          (Util.of_option_exn (Json.lookup j "supported_products"))
    ; new_supported_products =
        NewSupportedProductsList.of_json
          (Util.of_option_exn (Json.lookup j "new_supported_products"))
    ; applications =
        ApplicationList.of_json (Util.of_option_exn (Json.lookup j "applications"))
    ; configurations =
        ConfigurationList.of_json (Util.of_option_exn (Json.lookup j "configurations"))
    ; visible_to_all_users =
        Util.option_map (Json.lookup j "visible_to_all_users") Boolean.of_json
    ; job_flow_role = Util.option_map (Json.lookup j "job_flow_role") String.of_json
    ; service_role = Util.option_map (Json.lookup j "service_role") String.of_json
    ; tags = TagList.of_json (Util.of_option_exn (Json.lookup j "tags"))
    ; security_configuration =
        Util.option_map (Json.lookup j "security_configuration") String.of_json
    ; auto_scaling_role =
        Util.option_map (Json.lookup j "auto_scaling_role") String.of_json
    ; scale_down_behavior =
        Util.option_map (Json.lookup j "scale_down_behavior") ScaleDownBehavior.of_json
    ; custom_ami_id = Util.option_map (Json.lookup j "custom_ami_id") String.of_json
    ; ebs_root_volume_size =
        Util.option_map (Json.lookup j "ebs_root_volume_size") Integer.of_json
    ; repo_upgrade_on_boot =
        Util.option_map (Json.lookup j "repo_upgrade_on_boot") RepoUpgradeOnBoot.of_json
    ; kerberos_attributes =
        Util.option_map (Json.lookup j "kerberos_attributes") KerberosAttributes.of_json
    }
end

module ListSecurityConfigurationsOutput = struct
  type t =
    { security_configurations : SecurityConfigurationList.t
    ; marker : String.t option
    }

  let make ?(security_configurations = []) ?marker () =
    { security_configurations; marker }

  let parse xml =
    Some
      { security_configurations =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "SecurityConfigurations" xml)
               SecurityConfigurationList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ( "SecurityConfigurations.member"
                , SecurityConfigurationList.to_query v.security_configurations ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some
             ( "security_configurations"
             , SecurityConfigurationList.to_json v.security_configurations )
         ])

  let of_json j =
    { security_configurations =
        SecurityConfigurationList.of_json
          (Util.of_option_exn (Json.lookup j "security_configurations"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module RemoveTagsInput = struct
  type t =
    { resource_id : String.t
    ; tag_keys : StringList.t
    }

  let make ~resource_id ~tag_keys () = { resource_id; tag_keys }

  let parse xml =
    Some
      { resource_id =
          Xml.required
            "ResourceId"
            (Util.option_bind (Xml.member "ResourceId" xml) String.parse)
      ; tag_keys =
          Xml.required
            "TagKeys"
            (Util.option_bind (Xml.member "TagKeys" xml) StringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("TagKeys.member", StringList.to_query v.tag_keys))
         ; Some (Query.Pair ("ResourceId", String.to_query v.resource_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("tag_keys", StringList.to_json v.tag_keys)
         ; Some ("resource_id", String.to_json v.resource_id)
         ])

  let of_json j =
    { resource_id = String.of_json (Util.of_option_exn (Json.lookup j "resource_id"))
    ; tag_keys = StringList.of_json (Util.of_option_exn (Json.lookup j "tag_keys"))
    }
end

module ListInstanceFleetsOutput = struct
  type t =
    { instance_fleets : InstanceFleetList.t
    ; marker : String.t option
    }

  let make ?(instance_fleets = []) ?marker () = { instance_fleets; marker }

  let parse xml =
    Some
      { instance_fleets =
          Util.of_option
            []
            (Util.option_bind (Xml.member "InstanceFleets" xml) InstanceFleetList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ("InstanceFleets.member", InstanceFleetList.to_query v.instance_fleets))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("instance_fleets", InstanceFleetList.to_json v.instance_fleets)
         ])

  let of_json j =
    { instance_fleets =
        InstanceFleetList.of_json (Util.of_option_exn (Json.lookup j "instance_fleets"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module DeleteSecurityConfigurationOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module ListStepsOutput = struct
  type t =
    { steps : StepSummaryList.t
    ; marker : String.t option
    }

  let make ?(steps = []) ?marker () = { steps; marker }

  let parse xml =
    Some
      { steps =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Steps" xml) StepSummaryList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Steps.member", StepSummaryList.to_query v.steps))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("steps", StepSummaryList.to_json v.steps)
         ])

  let of_json j =
    { steps = StepSummaryList.of_json (Util.of_option_exn (Json.lookup j "steps"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module ModifyInstanceGroupsInput = struct
  type t =
    { cluster_id : String.t option
    ; instance_groups : InstanceGroupModifyConfigList.t
    }

  let make ?cluster_id ?(instance_groups = []) () = { cluster_id; instance_groups }

  let parse xml =
    Some
      { cluster_id = Util.option_bind (Xml.member "ClusterId" xml) String.parse
      ; instance_groups =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstanceGroups" xml)
               InstanceGroupModifyConfigList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "InstanceGroups.member"
                , InstanceGroupModifyConfigList.to_query v.instance_groups ))
         ; Util.option_map v.cluster_id (fun f ->
               Query.Pair ("ClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ("instance_groups", InstanceGroupModifyConfigList.to_json v.instance_groups)
         ; Util.option_map v.cluster_id (fun f -> "cluster_id", String.to_json f)
         ])

  let of_json j =
    { cluster_id = Util.option_map (Json.lookup j "cluster_id") String.of_json
    ; instance_groups =
        InstanceGroupModifyConfigList.of_json
          (Util.of_option_exn (Json.lookup j "instance_groups"))
    }
end

module AddJobFlowStepsInput = struct
  type t =
    { job_flow_id : String.t
    ; steps : StepConfigList.t
    }

  let make ~job_flow_id ~steps () = { job_flow_id; steps }

  let parse xml =
    Some
      { job_flow_id =
          Xml.required
            "JobFlowId"
            (Util.option_bind (Xml.member "JobFlowId" xml) String.parse)
      ; steps =
          Xml.required
            "Steps"
            (Util.option_bind (Xml.member "Steps" xml) StepConfigList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("Steps.member", StepConfigList.to_query v.steps))
         ; Some (Query.Pair ("JobFlowId", String.to_query v.job_flow_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("steps", StepConfigList.to_json v.steps)
         ; Some ("job_flow_id", String.to_json v.job_flow_id)
         ])

  let of_json j =
    { job_flow_id = String.of_json (Util.of_option_exn (Json.lookup j "job_flow_id"))
    ; steps = StepConfigList.of_json (Util.of_option_exn (Json.lookup j "steps"))
    }
end

module ListInstanceGroupsOutput = struct
  type t =
    { instance_groups : InstanceGroupList.t
    ; marker : String.t option
    }

  let make ?(instance_groups = []) ?marker () = { instance_groups; marker }

  let parse xml =
    Some
      { instance_groups =
          Util.of_option
            []
            (Util.option_bind (Xml.member "InstanceGroups" xml) InstanceGroupList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ("InstanceGroups.member", InstanceGroupList.to_query v.instance_groups))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("instance_groups", InstanceGroupList.to_json v.instance_groups)
         ])

  let of_json j =
    { instance_groups =
        InstanceGroupList.of_json (Util.of_option_exn (Json.lookup j "instance_groups"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module SetTerminationProtectionInput = struct
  type t =
    { job_flow_ids : XmlStringList.t
    ; termination_protected : Boolean.t
    }

  let make ~job_flow_ids ~termination_protected () =
    { job_flow_ids; termination_protected }

  let parse xml =
    Some
      { job_flow_ids =
          Xml.required
            "JobFlowIds"
            (Util.option_bind (Xml.member "JobFlowIds" xml) XmlStringList.parse)
      ; termination_protected =
          Xml.required
            "TerminationProtected"
            (Util.option_bind (Xml.member "TerminationProtected" xml) Boolean.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("TerminationProtected", Boolean.to_query v.termination_protected))
         ; Some (Query.Pair ("JobFlowIds.member", XmlStringList.to_query v.job_flow_ids))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("termination_protected", Boolean.to_json v.termination_protected)
         ; Some ("job_flow_ids", XmlStringList.to_json v.job_flow_ids)
         ])

  let of_json j =
    { job_flow_ids =
        XmlStringList.of_json (Util.of_option_exn (Json.lookup j "job_flow_ids"))
    ; termination_protected =
        Boolean.of_json (Util.of_option_exn (Json.lookup j "termination_protected"))
    }
end

module AddJobFlowStepsOutput = struct
  type t = { step_ids : StepIdsList.t }

  let make ?(step_ids = []) () = { step_ids }

  let parse xml =
    Some
      { step_ids =
          Util.of_option
            []
            (Util.option_bind (Xml.member "StepIds" xml) StepIdsList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("StepIds.member", StepIdsList.to_query v.step_ids)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("step_ids", StepIdsList.to_json v.step_ids) ])

  let of_json j =
    { step_ids = StepIdsList.of_json (Util.of_option_exn (Json.lookup j "step_ids")) }
end

module ListInstancesOutput = struct
  type t =
    { instances : InstanceList.t
    ; marker : String.t option
    }

  let make ?(instances = []) ?marker () = { instances; marker }

  let parse xml =
    Some
      { instances =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Instances" xml) InstanceList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Instances.member", InstanceList.to_query v.instances))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("instances", InstanceList.to_json v.instances)
         ])

  let of_json j =
    { instances = InstanceList.of_json (Util.of_option_exn (Json.lookup j "instances"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module InternalServerException = struct
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

module ListClustersOutput = struct
  type t =
    { clusters : ClusterSummaryList.t
    ; marker : String.t option
    }

  let make ?(clusters = []) ?marker () = { clusters; marker }

  let parse xml =
    Some
      { clusters =
          Util.of_option
            []
            (Util.option_bind (Xml.member "Clusters" xml) ClusterSummaryList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("Clusters.member", ClusterSummaryList.to_query v.clusters))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("clusters", ClusterSummaryList.to_json v.clusters)
         ])

  let of_json j =
    { clusters =
        ClusterSummaryList.of_json (Util.of_option_exn (Json.lookup j "clusters"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module InvalidRequestException = struct
  type t =
    { error_code : String.t option
    ; message : String.t option
    }

  let make ?error_code ?message () = { error_code; message }

  let parse xml =
    Some
      { error_code = Util.option_bind (Xml.member "ErrorCode" xml) String.parse
      ; message = Util.option_bind (Xml.member "Message" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> Query.Pair ("Message", String.to_query f))
         ; Util.option_map v.error_code (fun f ->
               Query.Pair ("ErrorCode", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.message (fun f -> "message", String.to_json f)
         ; Util.option_map v.error_code (fun f -> "error_code", String.to_json f)
         ])

  let of_json j =
    { error_code = Util.option_map (Json.lookup j "error_code") String.of_json
    ; message = Util.option_map (Json.lookup j "message") String.of_json
    }
end

module ListStepsInput = struct
  type t =
    { cluster_id : String.t
    ; step_states : StepStateList.t
    ; step_ids : XmlStringList.t
    ; marker : String.t option
    }

  let make ~cluster_id ?(step_states = []) ?(step_ids = []) ?marker () =
    { cluster_id; step_states; step_ids; marker }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; step_states =
          Util.of_option
            []
            (Util.option_bind (Xml.member "StepStates" xml) StepStateList.parse)
      ; step_ids =
          Util.of_option
            []
            (Util.option_bind (Xml.member "StepIds" xml) XmlStringList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some (Query.Pair ("StepIds.member", XmlStringList.to_query v.step_ids))
         ; Some (Query.Pair ("StepStates.member", StepStateList.to_query v.step_states))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("step_ids", XmlStringList.to_json v.step_ids)
         ; Some ("step_states", StepStateList.to_json v.step_states)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; step_states =
        StepStateList.of_json (Util.of_option_exn (Json.lookup j "step_states"))
    ; step_ids = XmlStringList.of_json (Util.of_option_exn (Json.lookup j "step_ids"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module RemoveAutoScalingPolicyOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module AddInstanceFleetOutput = struct
  type t =
    { cluster_id : String.t option
    ; instance_fleet_id : String.t option
    }

  let make ?cluster_id ?instance_fleet_id () = { cluster_id; instance_fleet_id }

  let parse xml =
    Some
      { cluster_id = Util.option_bind (Xml.member "ClusterId" xml) String.parse
      ; instance_fleet_id =
          Util.option_bind (Xml.member "InstanceFleetId" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.instance_fleet_id (fun f ->
               Query.Pair ("InstanceFleetId", String.to_query f))
         ; Util.option_map v.cluster_id (fun f ->
               Query.Pair ("ClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.instance_fleet_id (fun f ->
               "instance_fleet_id", String.to_json f)
         ; Util.option_map v.cluster_id (fun f -> "cluster_id", String.to_json f)
         ])

  let of_json j =
    { cluster_id = Util.option_map (Json.lookup j "cluster_id") String.of_json
    ; instance_fleet_id =
        Util.option_map (Json.lookup j "instance_fleet_id") String.of_json
    }
end

module InternalServerError = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module PutAutoScalingPolicyOutput = struct
  type t =
    { cluster_id : String.t option
    ; instance_group_id : String.t option
    ; auto_scaling_policy : AutoScalingPolicyDescription.t option
    }

  let make ?cluster_id ?instance_group_id ?auto_scaling_policy () =
    { cluster_id; instance_group_id; auto_scaling_policy }

  let parse xml =
    Some
      { cluster_id = Util.option_bind (Xml.member "ClusterId" xml) String.parse
      ; instance_group_id =
          Util.option_bind (Xml.member "InstanceGroupId" xml) String.parse
      ; auto_scaling_policy =
          Util.option_bind
            (Xml.member "AutoScalingPolicy" xml)
            AutoScalingPolicyDescription.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.auto_scaling_policy (fun f ->
               Query.Pair ("AutoScalingPolicy", AutoScalingPolicyDescription.to_query f))
         ; Util.option_map v.instance_group_id (fun f ->
               Query.Pair ("InstanceGroupId", String.to_query f))
         ; Util.option_map v.cluster_id (fun f ->
               Query.Pair ("ClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.auto_scaling_policy (fun f ->
               "auto_scaling_policy", AutoScalingPolicyDescription.to_json f)
         ; Util.option_map v.instance_group_id (fun f ->
               "instance_group_id", String.to_json f)
         ; Util.option_map v.cluster_id (fun f -> "cluster_id", String.to_json f)
         ])

  let of_json j =
    { cluster_id = Util.option_map (Json.lookup j "cluster_id") String.of_json
    ; instance_group_id =
        Util.option_map (Json.lookup j "instance_group_id") String.of_json
    ; auto_scaling_policy =
        Util.option_map
          (Json.lookup j "auto_scaling_policy")
          AutoScalingPolicyDescription.of_json
    }
end

module DescribeJobFlowsInput = struct
  type t =
    { created_after : DateTime.t option
    ; created_before : DateTime.t option
    ; job_flow_ids : XmlStringList.t
    ; job_flow_states : JobFlowExecutionStateList.t
    }

  let make ?created_after ?created_before ?(job_flow_ids = []) ?(job_flow_states = []) ()
      =
    { created_after; created_before; job_flow_ids; job_flow_states }

  let parse xml =
    Some
      { created_after = Util.option_bind (Xml.member "CreatedAfter" xml) DateTime.parse
      ; created_before = Util.option_bind (Xml.member "CreatedBefore" xml) DateTime.parse
      ; job_flow_ids =
          Util.of_option
            []
            (Util.option_bind (Xml.member "JobFlowIds" xml) XmlStringList.parse)
      ; job_flow_states =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "JobFlowStates" xml)
               JobFlowExecutionStateList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "JobFlowStates.member"
                , JobFlowExecutionStateList.to_query v.job_flow_states ))
         ; Some (Query.Pair ("JobFlowIds.member", XmlStringList.to_query v.job_flow_ids))
         ; Util.option_map v.created_before (fun f ->
               Query.Pair ("CreatedBefore", DateTime.to_query f))
         ; Util.option_map v.created_after (fun f ->
               Query.Pair ("CreatedAfter", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("job_flow_states", JobFlowExecutionStateList.to_json v.job_flow_states)
         ; Some ("job_flow_ids", XmlStringList.to_json v.job_flow_ids)
         ; Util.option_map v.created_before (fun f ->
               "created_before", DateTime.to_json f)
         ; Util.option_map v.created_after (fun f -> "created_after", DateTime.to_json f)
         ])

  let of_json j =
    { created_after = Util.option_map (Json.lookup j "created_after") DateTime.of_json
    ; created_before = Util.option_map (Json.lookup j "created_before") DateTime.of_json
    ; job_flow_ids =
        XmlStringList.of_json (Util.of_option_exn (Json.lookup j "job_flow_ids"))
    ; job_flow_states =
        JobFlowExecutionStateList.of_json
          (Util.of_option_exn (Json.lookup j "job_flow_states"))
    }
end

module AddTagsOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module CreateSecurityConfigurationOutput = struct
  type t =
    { name : String.t
    ; creation_date_time : DateTime.t
    }

  let make ~name ~creation_date_time () = { name; creation_date_time }

  let parse xml =
    Some
      { name = Xml.required "Name" (Util.option_bind (Xml.member "Name" xml) String.parse)
      ; creation_date_time =
          Xml.required
            "CreationDateTime"
            (Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("CreationDateTime", DateTime.to_query v.creation_date_time))
         ; Some (Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("creation_date_time", DateTime.to_json v.creation_date_time)
         ; Some ("name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Util.of_option_exn (Json.lookup j "name"))
    ; creation_date_time =
        DateTime.of_json (Util.of_option_exn (Json.lookup j "creation_date_time"))
    }
end

module ListSecurityConfigurationsInput = struct
  type t = { marker : String.t option }

  let make ?marker () = { marker }

  let parse xml =
    Some { marker = Util.option_bind (Xml.member "Marker" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f) ])

  let of_json j = { marker = Util.option_map (Json.lookup j "marker") String.of_json }
end

module RemoveTagsOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Query.List (Util.list_filter_opt [])

  let to_json v = `Assoc (Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeJobFlowsOutput = struct
  type t = { job_flows : JobFlowDetailList.t }

  let make ?(job_flows = []) () = { job_flows }

  let parse xml =
    Some
      { job_flows =
          Util.of_option
            []
            (Util.option_bind (Xml.member "JobFlows" xml) JobFlowDetailList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("JobFlows.member", JobFlowDetailList.to_query v.job_flows)) ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt [ Some ("job_flows", JobFlowDetailList.to_json v.job_flows) ])

  let of_json j =
    { job_flows =
        JobFlowDetailList.of_json (Util.of_option_exn (Json.lookup j "job_flows"))
    }
end

module ListBootstrapActionsOutput = struct
  type t =
    { bootstrap_actions : CommandList.t
    ; marker : String.t option
    }

  let make ?(bootstrap_actions = []) ?marker () = { bootstrap_actions; marker }

  let parse xml =
    Some
      { bootstrap_actions =
          Util.of_option
            []
            (Util.option_bind (Xml.member "BootstrapActions" xml) CommandList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ("BootstrapActions.member", CommandList.to_query v.bootstrap_actions))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("bootstrap_actions", CommandList.to_json v.bootstrap_actions)
         ])

  let of_json j =
    { bootstrap_actions =
        CommandList.of_json (Util.of_option_exn (Json.lookup j "bootstrap_actions"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module RunJobFlowOutput = struct
  type t = { job_flow_id : String.t option }

  let make ?job_flow_id () = { job_flow_id }

  let parse xml =
    Some { job_flow_id = Util.option_bind (Xml.member "JobFlowId" xml) String.parse }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.job_flow_id (fun f ->
               Query.Pair ("JobFlowId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.job_flow_id (fun f -> "job_flow_id", String.to_json f) ])

  let of_json j =
    { job_flow_id = Util.option_map (Json.lookup j "job_flow_id") String.of_json }
end

module AddInstanceGroupsOutput = struct
  type t =
    { job_flow_id : String.t option
    ; instance_group_ids : InstanceGroupIdsList.t
    }

  let make ?job_flow_id ?(instance_group_ids = []) () =
    { job_flow_id; instance_group_ids }

  let parse xml =
    Some
      { job_flow_id = Util.option_bind (Xml.member "JobFlowId" xml) String.parse
      ; instance_group_ids =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstanceGroupIds" xml)
               InstanceGroupIdsList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "InstanceGroupIds.member"
                , InstanceGroupIdsList.to_query v.instance_group_ids ))
         ; Util.option_map v.job_flow_id (fun f ->
               Query.Pair ("JobFlowId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("instance_group_ids", InstanceGroupIdsList.to_json v.instance_group_ids)
         ; Util.option_map v.job_flow_id (fun f -> "job_flow_id", String.to_json f)
         ])

  let of_json j =
    { job_flow_id = Util.option_map (Json.lookup j "job_flow_id") String.of_json
    ; instance_group_ids =
        InstanceGroupIdsList.of_json
          (Util.of_option_exn (Json.lookup j "instance_group_ids"))
    }
end

module RemoveAutoScalingPolicyInput = struct
  type t =
    { cluster_id : String.t
    ; instance_group_id : String.t
    }

  let make ~cluster_id ~instance_group_id () = { cluster_id; instance_group_id }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; instance_group_id =
          Xml.required
            "InstanceGroupId"
            (Util.option_bind (Xml.member "InstanceGroupId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("InstanceGroupId", String.to_query v.instance_group_id))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("instance_group_id", String.to_json v.instance_group_id)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; instance_group_id =
        String.of_json (Util.of_option_exn (Json.lookup j "instance_group_id"))
    }
end

module DescribeSecurityConfigurationOutput = struct
  type t =
    { name : String.t option
    ; security_configuration : String.t option
    ; creation_date_time : DateTime.t option
    }

  let make ?name ?security_configuration ?creation_date_time () =
    { name; security_configuration; creation_date_time }

  let parse xml =
    Some
      { name = Util.option_bind (Xml.member "Name" xml) String.parse
      ; security_configuration =
          Util.option_bind (Xml.member "SecurityConfiguration" xml) String.parse
      ; creation_date_time =
          Util.option_bind (Xml.member "CreationDateTime" xml) DateTime.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.creation_date_time (fun f ->
               Query.Pair ("CreationDateTime", DateTime.to_query f))
         ; Util.option_map v.security_configuration (fun f ->
               Query.Pair ("SecurityConfiguration", String.to_query f))
         ; Util.option_map v.name (fun f -> Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.creation_date_time (fun f ->
               "creation_date_time", DateTime.to_json f)
         ; Util.option_map v.security_configuration (fun f ->
               "security_configuration", String.to_json f)
         ; Util.option_map v.name (fun f -> "name", String.to_json f)
         ])

  let of_json j =
    { name = Util.option_map (Json.lookup j "name") String.of_json
    ; security_configuration =
        Util.option_map (Json.lookup j "security_configuration") String.of_json
    ; creation_date_time =
        Util.option_map (Json.lookup j "creation_date_time") DateTime.of_json
    }
end

module TerminateJobFlowsInput = struct
  type t = { job_flow_ids : XmlStringList.t }

  let make ~job_flow_ids () = { job_flow_ids }

  let parse xml =
    Some
      { job_flow_ids =
          Xml.required
            "JobFlowIds"
            (Util.option_bind (Xml.member "JobFlowIds" xml) XmlStringList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("JobFlowIds.member", XmlStringList.to_query v.job_flow_ids))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("job_flow_ids", XmlStringList.to_json v.job_flow_ids) ])

  let of_json j =
    { job_flow_ids =
        XmlStringList.of_json (Util.of_option_exn (Json.lookup j "job_flow_ids"))
    }
end

module CancelStepsOutput = struct
  type t = { cancel_steps_info_list : CancelStepsInfoList.t }

  let make ?(cancel_steps_info_list = []) () = { cancel_steps_info_list }

  let parse xml =
    Some
      { cancel_steps_info_list =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "CancelStepsInfoList" xml)
               CancelStepsInfoList.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ( "CancelStepsInfoList.member"
                , CancelStepsInfoList.to_query v.cancel_steps_info_list ))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some
             ( "cancel_steps_info_list"
             , CancelStepsInfoList.to_json v.cancel_steps_info_list )
         ])

  let of_json j =
    { cancel_steps_info_list =
        CancelStepsInfoList.of_json
          (Util.of_option_exn (Json.lookup j "cancel_steps_info_list"))
    }
end

module DescribeClusterInput = struct
  type t = { cluster_id : String.t }

  let make ~cluster_id () = { cluster_id }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some (Query.Pair ("ClusterId", String.to_query v.cluster_id)) ])

  let to_json v =
    `Assoc (Util.list_filter_opt [ Some ("cluster_id", String.to_json v.cluster_id) ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id")) }
end

module PutAutoScalingPolicyInput = struct
  type t =
    { cluster_id : String.t
    ; instance_group_id : String.t
    ; auto_scaling_policy : AutoScalingPolicy.t
    }

  let make ~cluster_id ~instance_group_id ~auto_scaling_policy () =
    { cluster_id; instance_group_id; auto_scaling_policy }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; instance_group_id =
          Xml.required
            "InstanceGroupId"
            (Util.option_bind (Xml.member "InstanceGroupId" xml) String.parse)
      ; auto_scaling_policy =
          Xml.required
            "AutoScalingPolicy"
            (Util.option_bind
               (Xml.member "AutoScalingPolicy" xml)
               AutoScalingPolicy.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair
                ("AutoScalingPolicy", AutoScalingPolicy.to_query v.auto_scaling_policy))
         ; Some (Query.Pair ("InstanceGroupId", String.to_query v.instance_group_id))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("auto_scaling_policy", AutoScalingPolicy.to_json v.auto_scaling_policy)
         ; Some ("instance_group_id", String.to_json v.instance_group_id)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; instance_group_id =
        String.of_json (Util.of_option_exn (Json.lookup j "instance_group_id"))
    ; auto_scaling_policy =
        AutoScalingPolicy.of_json
          (Util.of_option_exn (Json.lookup j "auto_scaling_policy"))
    }
end

module ListInstancesInput = struct
  type t =
    { cluster_id : String.t
    ; instance_group_id : String.t option
    ; instance_group_types : InstanceGroupTypeList.t
    ; instance_fleet_id : String.t option
    ; instance_fleet_type : InstanceFleetType.t option
    ; instance_states : InstanceStateList.t
    ; marker : String.t option
    }

  let make
      ~cluster_id
      ?instance_group_id
      ?(instance_group_types = [])
      ?instance_fleet_id
      ?instance_fleet_type
      ?(instance_states = [])
      ?marker
      () =
    { cluster_id
    ; instance_group_id
    ; instance_group_types
    ; instance_fleet_id
    ; instance_fleet_type
    ; instance_states
    ; marker
    }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; instance_group_id =
          Util.option_bind (Xml.member "InstanceGroupId" xml) String.parse
      ; instance_group_types =
          Util.of_option
            []
            (Util.option_bind
               (Xml.member "InstanceGroupTypes" xml)
               InstanceGroupTypeList.parse)
      ; instance_fleet_id =
          Util.option_bind (Xml.member "InstanceFleetId" xml) String.parse
      ; instance_fleet_type =
          Util.option_bind (Xml.member "InstanceFleetType" xml) InstanceFleetType.parse
      ; instance_states =
          Util.of_option
            []
            (Util.option_bind (Xml.member "InstanceStates" xml) InstanceStateList.parse)
      ; marker = Util.option_bind (Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> Query.Pair ("Marker", String.to_query f))
         ; Some
             (Query.Pair
                ("InstanceStates.member", InstanceStateList.to_query v.instance_states))
         ; Util.option_map v.instance_fleet_type (fun f ->
               Query.Pair ("InstanceFleetType", InstanceFleetType.to_query f))
         ; Util.option_map v.instance_fleet_id (fun f ->
               Query.Pair ("InstanceFleetId", String.to_query f))
         ; Some
             (Query.Pair
                ( "InstanceGroupTypes.member"
                , InstanceGroupTypeList.to_query v.instance_group_types ))
         ; Util.option_map v.instance_group_id (fun f ->
               Query.Pair ("InstanceGroupId", String.to_query f))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Util.option_map v.marker (fun f -> "marker", String.to_json f)
         ; Some ("instance_states", InstanceStateList.to_json v.instance_states)
         ; Util.option_map v.instance_fleet_type (fun f ->
               "instance_fleet_type", InstanceFleetType.to_json f)
         ; Util.option_map v.instance_fleet_id (fun f ->
               "instance_fleet_id", String.to_json f)
         ; Some
             ("instance_group_types", InstanceGroupTypeList.to_json v.instance_group_types)
         ; Util.option_map v.instance_group_id (fun f ->
               "instance_group_id", String.to_json f)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; instance_group_id =
        Util.option_map (Json.lookup j "instance_group_id") String.of_json
    ; instance_group_types =
        InstanceGroupTypeList.of_json
          (Util.of_option_exn (Json.lookup j "instance_group_types"))
    ; instance_fleet_id =
        Util.option_map (Json.lookup j "instance_fleet_id") String.of_json
    ; instance_fleet_type =
        Util.option_map (Json.lookup j "instance_fleet_type") InstanceFleetType.of_json
    ; instance_states =
        InstanceStateList.of_json (Util.of_option_exn (Json.lookup j "instance_states"))
    ; marker = Util.option_map (Json.lookup j "marker") String.of_json
    }
end

module AddInstanceFleetInput = struct
  type t =
    { cluster_id : String.t
    ; instance_fleet : InstanceFleetConfig.t
    }

  let make ~cluster_id ~instance_fleet () = { cluster_id; instance_fleet }

  let parse xml =
    Some
      { cluster_id =
          Xml.required
            "ClusterId"
            (Util.option_bind (Xml.member "ClusterId" xml) String.parse)
      ; instance_fleet =
          Xml.required
            "InstanceFleet"
            (Util.option_bind (Xml.member "InstanceFleet" xml) InstanceFleetConfig.parse)
      }

  let to_query v =
    Query.List
      (Util.list_filter_opt
         [ Some
             (Query.Pair ("InstanceFleet", InstanceFleetConfig.to_query v.instance_fleet))
         ; Some (Query.Pair ("ClusterId", String.to_query v.cluster_id))
         ])

  let to_json v =
    `Assoc
      (Util.list_filter_opt
         [ Some ("instance_fleet", InstanceFleetConfig.to_json v.instance_fleet)
         ; Some ("cluster_id", String.to_json v.cluster_id)
         ])

  let of_json j =
    { cluster_id = String.of_json (Util.of_option_exn (Json.lookup j "cluster_id"))
    ; instance_fleet =
        InstanceFleetConfig.of_json (Util.of_option_exn (Json.lookup j "instance_fleet"))
    }
end
