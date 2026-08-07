open Aws.BaseTypes

type t =
  { name : String.t
  ; include_filters : MetricStreamFilters.t
  ; exclude_filters : MetricStreamFilters.t
  ; firehose_arn : String.t
  ; role_arn : String.t
  ; output_format : MetricStreamOutputFormat.t
  ; tags : TagList.t
  ; statistics_configurations : MetricStreamStatisticsConfigurations.t
  ; include_linked_accounts_metrics : Boolean.t option
  }

let make
    ~name
    ?(include_filters = [])
    ?(exclude_filters = [])
    ~firehose_arn
    ~role_arn
    ~output_format
    ?(tags = [])
    ?(statistics_configurations = [])
    ?include_linked_accounts_metrics
    () =
  { name
  ; include_filters
  ; exclude_filters
  ; firehose_arn
  ; role_arn
  ; output_format
  ; tags
  ; statistics_configurations
  ; include_linked_accounts_metrics
  }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; include_filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "IncludeFilters" xml)
             MetricStreamFilters.parse)
    ; exclude_filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ExcludeFilters" xml)
             MetricStreamFilters.parse)
    ; firehose_arn =
        Aws.Xml.required
          "FirehoseArn"
          (Aws.Util.option_bind (Aws.Xml.member "FirehoseArn" xml) String.parse)
    ; role_arn =
        Aws.Xml.required
          "RoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse)
    ; output_format =
        Aws.Xml.required
          "OutputFormat"
          (Aws.Util.option_bind
             (Aws.Xml.member "OutputFormat" xml)
             MetricStreamOutputFormat.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; statistics_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StatisticsConfigurations" xml)
             MetricStreamStatisticsConfigurations.parse)
    ; include_linked_accounts_metrics =
        Aws.Util.option_bind
          (Aws.Xml.member "IncludeLinkedAccountsMetrics" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_linked_accounts_metrics (fun f ->
             Aws.Query.Pair ("IncludeLinkedAccountsMetrics", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "StatisticsConfigurations.member"
              , MetricStreamStatisticsConfigurations.to_query v.statistics_configurations
              ))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("OutputFormat", MetricStreamOutputFormat.to_query v.output_format))
       ; Some (Aws.Query.Pair ("RoleArn", String.to_query v.role_arn))
       ; Some (Aws.Query.Pair ("FirehoseArn", String.to_query v.firehose_arn))
       ; Some
           (Aws.Query.Pair
              ("ExcludeFilters.member", MetricStreamFilters.to_query v.exclude_filters))
       ; Some
           (Aws.Query.Pair
              ("IncludeFilters.member", MetricStreamFilters.to_query v.include_filters))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_linked_accounts_metrics (fun f ->
             "IncludeLinkedAccountsMetrics", Boolean.to_json f)
       ; Some
           ( "StatisticsConfigurations"
           , MetricStreamStatisticsConfigurations.to_json v.statistics_configurations )
       ; Some ("Tags", TagList.to_json v.tags)
       ; Some ("OutputFormat", MetricStreamOutputFormat.to_json v.output_format)
       ; Some ("RoleArn", String.to_json v.role_arn)
       ; Some ("FirehoseArn", String.to_json v.firehose_arn)
       ; Some ("ExcludeFilters", MetricStreamFilters.to_json v.exclude_filters)
       ; Some ("IncludeFilters", MetricStreamFilters.to_json v.include_filters)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; include_filters =
      MetricStreamFilters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IncludeFilters"))
  ; exclude_filters =
      MetricStreamFilters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludeFilters"))
  ; firehose_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FirehoseArn"))
  ; role_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleArn"))
  ; output_format =
      MetricStreamOutputFormat.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OutputFormat"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; statistics_configurations =
      MetricStreamStatisticsConfigurations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StatisticsConfigurations"))
  ; include_linked_accounts_metrics =
      Aws.Util.option_map
        (Aws.Json.lookup j "IncludeLinkedAccountsMetrics")
        Boolean.of_json
  }
