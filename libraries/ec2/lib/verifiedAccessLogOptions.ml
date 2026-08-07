open Aws.BaseTypes

type t =
  { s3 : VerifiedAccessLogS3DestinationOptions.t option
  ; cloud_watch_logs : VerifiedAccessLogCloudWatchLogsDestinationOptions.t option
  ; kinesis_data_firehose :
      VerifiedAccessLogKinesisDataFirehoseDestinationOptions.t option
  ; log_version : String.t option
  ; include_trust_context : Boolean.t option
  }

let make
    ?s3
    ?cloud_watch_logs
    ?kinesis_data_firehose
    ?log_version
    ?include_trust_context
    () =
  { s3; cloud_watch_logs; kinesis_data_firehose; log_version; include_trust_context }

let parse xml =
  Some
    { s3 =
        Aws.Util.option_bind
          (Aws.Xml.member "S3" xml)
          VerifiedAccessLogS3DestinationOptions.parse
    ; cloud_watch_logs =
        Aws.Util.option_bind
          (Aws.Xml.member "CloudWatchLogs" xml)
          VerifiedAccessLogCloudWatchLogsDestinationOptions.parse
    ; kinesis_data_firehose =
        Aws.Util.option_bind
          (Aws.Xml.member "KinesisDataFirehose" xml)
          VerifiedAccessLogKinesisDataFirehoseDestinationOptions.parse
    ; log_version = Aws.Util.option_bind (Aws.Xml.member "LogVersion" xml) String.parse
    ; include_trust_context =
        Aws.Util.option_bind (Aws.Xml.member "IncludeTrustContext" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_trust_context (fun f ->
             Aws.Query.Pair ("IncludeTrustContext", Boolean.to_query f))
       ; Aws.Util.option_map v.log_version (fun f ->
             Aws.Query.Pair ("LogVersion", String.to_query f))
       ; Aws.Util.option_map v.kinesis_data_firehose (fun f ->
             Aws.Query.Pair
               ( "KinesisDataFirehose"
               , VerifiedAccessLogKinesisDataFirehoseDestinationOptions.to_query f ))
       ; Aws.Util.option_map v.cloud_watch_logs (fun f ->
             Aws.Query.Pair
               ( "CloudWatchLogs"
               , VerifiedAccessLogCloudWatchLogsDestinationOptions.to_query f ))
       ; Aws.Util.option_map v.s3 (fun f ->
             Aws.Query.Pair ("S3", VerifiedAccessLogS3DestinationOptions.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_trust_context (fun f ->
             "IncludeTrustContext", Boolean.to_json f)
       ; Aws.Util.option_map v.log_version (fun f -> "LogVersion", String.to_json f)
       ; Aws.Util.option_map v.kinesis_data_firehose (fun f ->
             ( "KinesisDataFirehose"
             , VerifiedAccessLogKinesisDataFirehoseDestinationOptions.to_json f ))
       ; Aws.Util.option_map v.cloud_watch_logs (fun f ->
             "CloudWatchLogs", VerifiedAccessLogCloudWatchLogsDestinationOptions.to_json f)
       ; Aws.Util.option_map v.s3 (fun f ->
             "S3", VerifiedAccessLogS3DestinationOptions.to_json f)
       ])

let of_json j =
  { s3 =
      Aws.Util.option_map
        (Aws.Json.lookup j "S3")
        VerifiedAccessLogS3DestinationOptions.of_json
  ; cloud_watch_logs =
      Aws.Util.option_map
        (Aws.Json.lookup j "CloudWatchLogs")
        VerifiedAccessLogCloudWatchLogsDestinationOptions.of_json
  ; kinesis_data_firehose =
      Aws.Util.option_map
        (Aws.Json.lookup j "KinesisDataFirehose")
        VerifiedAccessLogKinesisDataFirehoseDestinationOptions.of_json
  ; log_version = Aws.Util.option_map (Aws.Json.lookup j "LogVersion") String.of_json
  ; include_trust_context =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeTrustContext") Boolean.of_json
  }
