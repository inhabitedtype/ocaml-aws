open Aws.BaseTypes

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
  ; metric_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MetricName"))
  ; dimensions =
      Dimensions.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  ; start_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
  ; end_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EndTime"))
  ; period = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Period"))
  ; statistics =
      Statistics.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Statistics"))
  ; extended_statistics =
      ExtendedStatistics.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExtendedStatistics"))
  ; unit = Aws.Util.option_map (Aws.Json.lookup j "Unit") StandardUnit.of_json
  }
