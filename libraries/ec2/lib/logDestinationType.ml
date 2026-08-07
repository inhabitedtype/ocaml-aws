open Aws.BaseTypes

type t =
  | Cloud_watch_logs
  | S3
  | Kinesis_data_firehose

let str_to_t =
  [ "kinesis-data-firehose", Kinesis_data_firehose
  ; "s3", S3
  ; "cloud-watch-logs", Cloud_watch_logs
  ]

let t_to_str =
  [ Kinesis_data_firehose, "kinesis-data-firehose"
  ; S3, "s3"
  ; Cloud_watch_logs, "cloud-watch-logs"
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
