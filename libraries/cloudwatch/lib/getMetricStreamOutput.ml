open Aws.BaseTypes

type t =
  { arn : String.t option
  ; name : String.t option
  ; include_filters : MetricStreamFilters.t
  ; exclude_filters : MetricStreamFilters.t
  ; firehose_arn : String.t option
  ; role_arn : String.t option
  ; state : String.t option
  ; creation_date : DateTime.t option
  ; last_update_date : DateTime.t option
  ; output_format : MetricStreamOutputFormat.t option
  ; statistics_configurations : MetricStreamStatisticsConfigurations.t
  ; include_linked_accounts_metrics : Boolean.t option
  }

let make
    ?arn
    ?name
    ?(include_filters = [])
    ?(exclude_filters = [])
    ?firehose_arn
    ?role_arn
    ?state
    ?creation_date
    ?last_update_date
    ?output_format
    ?(statistics_configurations = [])
    ?include_linked_accounts_metrics
    () =
  { arn
  ; name
  ; include_filters
  ; exclude_filters
  ; firehose_arn
  ; role_arn
  ; state
  ; creation_date
  ; last_update_date
  ; output_format
  ; statistics_configurations
  ; include_linked_accounts_metrics
  }

let parse xml =
  Some
    { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
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
    ; firehose_arn = Aws.Util.option_bind (Aws.Xml.member "FirehoseArn" xml) String.parse
    ; role_arn = Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse
    ; creation_date =
        Aws.Util.option_bind (Aws.Xml.member "CreationDate" xml) DateTime.parse
    ; last_update_date =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdateDate" xml) DateTime.parse
    ; output_format =
        Aws.Util.option_bind
          (Aws.Xml.member "OutputFormat" xml)
          MetricStreamOutputFormat.parse
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
       ; Aws.Util.option_map v.output_format (fun f ->
             Aws.Query.Pair ("OutputFormat", MetricStreamOutputFormat.to_query f))
       ; Aws.Util.option_map v.last_update_date (fun f ->
             Aws.Query.Pair ("LastUpdateDate", DateTime.to_query f))
       ; Aws.Util.option_map v.creation_date (fun f ->
             Aws.Query.Pair ("CreationDate", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.role_arn (fun f ->
             Aws.Query.Pair ("RoleArn", String.to_query f))
       ; Aws.Util.option_map v.firehose_arn (fun f ->
             Aws.Query.Pair ("FirehoseArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ExcludeFilters.member", MetricStreamFilters.to_query v.exclude_filters))
       ; Some
           (Aws.Query.Pair
              ("IncludeFilters.member", MetricStreamFilters.to_query v.include_filters))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_linked_accounts_metrics (fun f ->
             "IncludeLinkedAccountsMetrics", Boolean.to_json f)
       ; Some
           ( "StatisticsConfigurations"
           , MetricStreamStatisticsConfigurations.to_json v.statistics_configurations )
       ; Aws.Util.option_map v.output_format (fun f ->
             "OutputFormat", MetricStreamOutputFormat.to_json f)
       ; Aws.Util.option_map v.last_update_date (fun f ->
             "LastUpdateDate", DateTime.to_json f)
       ; Aws.Util.option_map v.creation_date (fun f -> "CreationDate", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "State", String.to_json f)
       ; Aws.Util.option_map v.role_arn (fun f -> "RoleArn", String.to_json f)
       ; Aws.Util.option_map v.firehose_arn (fun f -> "FirehoseArn", String.to_json f)
       ; Some ("ExcludeFilters", MetricStreamFilters.to_json v.exclude_filters)
       ; Some ("IncludeFilters", MetricStreamFilters.to_json v.include_filters)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ])

let of_json j =
  { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; include_filters =
      MetricStreamFilters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IncludeFilters"))
  ; exclude_filters =
      MetricStreamFilters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludeFilters"))
  ; firehose_arn = Aws.Util.option_map (Aws.Json.lookup j "FirehoseArn") String.of_json
  ; role_arn = Aws.Util.option_map (Aws.Json.lookup j "RoleArn") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "State") String.of_json
  ; creation_date =
      Aws.Util.option_map (Aws.Json.lookup j "CreationDate") DateTime.of_json
  ; last_update_date =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdateDate") DateTime.of_json
  ; output_format =
      Aws.Util.option_map
        (Aws.Json.lookup j "OutputFormat")
        MetricStreamOutputFormat.of_json
  ; statistics_configurations =
      MetricStreamStatisticsConfigurations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StatisticsConfigurations"))
  ; include_linked_accounts_metrics =
      Aws.Util.option_map
        (Aws.Json.lookup j "IncludeLinkedAccountsMetrics")
        Boolean.of_json
  }
