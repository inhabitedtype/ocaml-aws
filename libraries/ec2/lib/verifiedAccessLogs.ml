open Aws.BaseTypes

type t =
  { s3 : VerifiedAccessLogS3Destination.t option
  ; cloud_watch_logs : VerifiedAccessLogCloudWatchLogsDestination.t option
  ; kinesis_data_firehose : VerifiedAccessLogKinesisDataFirehoseDestination.t option
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
          (Aws.Xml.member "s3" xml)
          VerifiedAccessLogS3Destination.parse
    ; cloud_watch_logs =
        Aws.Util.option_bind
          (Aws.Xml.member "cloudWatchLogs" xml)
          VerifiedAccessLogCloudWatchLogsDestination.parse
    ; kinesis_data_firehose =
        Aws.Util.option_bind
          (Aws.Xml.member "kinesisDataFirehose" xml)
          VerifiedAccessLogKinesisDataFirehoseDestination.parse
    ; log_version = Aws.Util.option_bind (Aws.Xml.member "logVersion" xml) String.parse
    ; include_trust_context =
        Aws.Util.option_bind (Aws.Xml.member "includeTrustContext" xml) Boolean.parse
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
               , VerifiedAccessLogKinesisDataFirehoseDestination.to_query f ))
       ; Aws.Util.option_map v.cloud_watch_logs (fun f ->
             Aws.Query.Pair
               ("CloudWatchLogs", VerifiedAccessLogCloudWatchLogsDestination.to_query f))
       ; Aws.Util.option_map v.s3 (fun f ->
             Aws.Query.Pair ("S3", VerifiedAccessLogS3Destination.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_trust_context (fun f ->
             "includeTrustContext", Boolean.to_json f)
       ; Aws.Util.option_map v.log_version (fun f -> "logVersion", String.to_json f)
       ; Aws.Util.option_map v.kinesis_data_firehose (fun f ->
             ( "kinesisDataFirehose"
             , VerifiedAccessLogKinesisDataFirehoseDestination.to_json f ))
       ; Aws.Util.option_map v.cloud_watch_logs (fun f ->
             "cloudWatchLogs", VerifiedAccessLogCloudWatchLogsDestination.to_json f)
       ; Aws.Util.option_map v.s3 (fun f ->
             "s3", VerifiedAccessLogS3Destination.to_json f)
       ])

let of_json j =
  { s3 =
      Aws.Util.option_map (Aws.Json.lookup j "s3") VerifiedAccessLogS3Destination.of_json
  ; cloud_watch_logs =
      Aws.Util.option_map
        (Aws.Json.lookup j "cloudWatchLogs")
        VerifiedAccessLogCloudWatchLogsDestination.of_json
  ; kinesis_data_firehose =
      Aws.Util.option_map
        (Aws.Json.lookup j "kinesisDataFirehose")
        VerifiedAccessLogKinesisDataFirehoseDestination.of_json
  ; log_version = Aws.Util.option_map (Aws.Json.lookup j "logVersion") String.of_json
  ; include_trust_context =
      Aws.Util.option_map (Aws.Json.lookup j "includeTrustContext") Boolean.of_json
  }
