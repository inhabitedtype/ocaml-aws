open Aws.BaseTypes

type t =
  { kms_key_id : String.t option
  ; kinesis_stream_name : String.t option
  ; status : ActivityStreamStatus.t option
  }

let make ?kms_key_id ?kinesis_stream_name ?status () =
  { kms_key_id; kinesis_stream_name; status }

let parse xml =
  Some
    { kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; kinesis_stream_name =
        Aws.Util.option_bind (Aws.Xml.member "KinesisStreamName" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) ActivityStreamStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ActivityStreamStatus.to_query f))
       ; Aws.Util.option_map v.kinesis_stream_name (fun f ->
             Aws.Query.Pair ("KinesisStreamName", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", ActivityStreamStatus.to_json f)
       ; Aws.Util.option_map v.kinesis_stream_name (fun f ->
             "KinesisStreamName", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ])

let of_json j =
  { kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; kinesis_stream_name =
      Aws.Util.option_map (Aws.Json.lookup j "KinesisStreamName") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") ActivityStreamStatus.of_json
  }
