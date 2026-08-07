open Aws.BaseTypes

type t =
  { evaluation_periods : Integer.t
  ; threshold : Double.t
  ; comparison_operator : ComparisonOperator.t
  ; period : Integer.t
  ; metric_name : String.t
  ; namespace : String.t
  ; statistic : Statistic.t
  ; dimensions : DimensionList.t
  }

let make
    ~evaluation_periods
    ~threshold
    ~comparison_operator
    ~period
    ~metric_name
    ~namespace
    ~statistic
    ?(dimensions = [])
    () =
  { evaluation_periods
  ; threshold
  ; comparison_operator
  ; period
  ; metric_name
  ; namespace
  ; statistic
  ; dimensions
  }

let parse xml =
  Some
    { evaluation_periods =
        Aws.Xml.required
          "EvaluationPeriods"
          (Aws.Util.option_bind (Aws.Xml.member "EvaluationPeriods" xml) Integer.parse)
    ; threshold =
        Aws.Xml.required
          "Threshold"
          (Aws.Util.option_bind (Aws.Xml.member "Threshold" xml) Double.parse)
    ; comparison_operator =
        Aws.Xml.required
          "ComparisonOperator"
          (Aws.Util.option_bind
             (Aws.Xml.member "ComparisonOperator" xml)
             ComparisonOperator.parse)
    ; period =
        Aws.Xml.required
          "Period"
          (Aws.Util.option_bind (Aws.Xml.member "Period" xml) Integer.parse)
    ; metric_name =
        Aws.Xml.required
          "MetricName"
          (Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse)
    ; namespace =
        Aws.Xml.required
          "Namespace"
          (Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse)
    ; statistic =
        Aws.Xml.required
          "Statistic"
          (Aws.Util.option_bind (Aws.Xml.member "Statistic" xml) Statistic.parse)
    ; dimensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) DimensionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Dimensions.member", DimensionList.to_query v.dimensions))
       ; Some (Aws.Query.Pair ("Statistic", Statistic.to_query v.statistic))
       ; Some (Aws.Query.Pair ("Namespace", String.to_query v.namespace))
       ; Some (Aws.Query.Pair ("MetricName", String.to_query v.metric_name))
       ; Some (Aws.Query.Pair ("Period", Integer.to_query v.period))
       ; Some
           (Aws.Query.Pair
              ("ComparisonOperator", ComparisonOperator.to_query v.comparison_operator))
       ; Some (Aws.Query.Pair ("Threshold", Double.to_query v.threshold))
       ; Some
           (Aws.Query.Pair ("EvaluationPeriods", Integer.to_query v.evaluation_periods))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Dimensions", DimensionList.to_json v.dimensions)
       ; Some ("Statistic", Statistic.to_json v.statistic)
       ; Some ("Namespace", String.to_json v.namespace)
       ; Some ("MetricName", String.to_json v.metric_name)
       ; Some ("Period", Integer.to_json v.period)
       ; Some ("ComparisonOperator", ComparisonOperator.to_json v.comparison_operator)
       ; Some ("Threshold", Double.to_json v.threshold)
       ; Some ("EvaluationPeriods", Integer.to_json v.evaluation_periods)
       ])

let of_json j =
  { evaluation_periods =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EvaluationPeriods"))
  ; threshold = Double.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Threshold"))
  ; comparison_operator =
      ComparisonOperator.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ComparisonOperator"))
  ; period = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Period"))
  ; metric_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
  ; namespace = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Namespace"))
  ; statistic = Statistic.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Statistic"))
  ; dimensions =
      DimensionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  }
