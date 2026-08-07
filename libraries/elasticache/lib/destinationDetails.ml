type t =
  { cloud_watch_logs_details : CloudWatchLogsDestinationDetails.t option
  ; kinesis_firehose_details : KinesisFirehoseDestinationDetails.t option
  }

let make ?cloud_watch_logs_details ?kinesis_firehose_details () =
  { cloud_watch_logs_details; kinesis_firehose_details }

let parse xml =
  Some
    { cloud_watch_logs_details =
        Aws.Util.option_bind
          (Aws.Xml.member "CloudWatchLogsDetails" xml)
          CloudWatchLogsDestinationDetails.parse
    ; kinesis_firehose_details =
        Aws.Util.option_bind
          (Aws.Xml.member "KinesisFirehoseDetails" xml)
          KinesisFirehoseDestinationDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kinesis_firehose_details (fun f ->
             Aws.Query.Pair
               ("KinesisFirehoseDetails", KinesisFirehoseDestinationDetails.to_query f))
       ; Aws.Util.option_map v.cloud_watch_logs_details (fun f ->
             Aws.Query.Pair
               ("CloudWatchLogsDetails", CloudWatchLogsDestinationDetails.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kinesis_firehose_details (fun f ->
             "KinesisFirehoseDetails", KinesisFirehoseDestinationDetails.to_json f)
       ; Aws.Util.option_map v.cloud_watch_logs_details (fun f ->
             "CloudWatchLogsDetails", CloudWatchLogsDestinationDetails.to_json f)
       ])

let of_json j =
  { cloud_watch_logs_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "CloudWatchLogsDetails")
        CloudWatchLogsDestinationDetails.of_json
  ; kinesis_firehose_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "KinesisFirehoseDetails")
        KinesisFirehoseDestinationDetails.of_json
  }
