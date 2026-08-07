open Aws.BaseTypes

type t =
  { ami_id : String.t option
  ; task_start_time : DateTime.t option
  ; bucket : String.t option
  ; s3object_key : String.t option
  ; progress_percentage : Integer.t option
  ; store_task_state : String.t option
  ; store_task_failure_reason : String.t option
  }

let make
    ?ami_id
    ?task_start_time
    ?bucket
    ?s3object_key
    ?progress_percentage
    ?store_task_state
    ?store_task_failure_reason
    () =
  { ami_id
  ; task_start_time
  ; bucket
  ; s3object_key
  ; progress_percentage
  ; store_task_state
  ; store_task_failure_reason
  }

let parse xml =
  Some
    { ami_id = Aws.Util.option_bind (Aws.Xml.member "amiId" xml) String.parse
    ; task_start_time =
        Aws.Util.option_bind (Aws.Xml.member "taskStartTime" xml) DateTime.parse
    ; bucket = Aws.Util.option_bind (Aws.Xml.member "bucket" xml) String.parse
    ; s3object_key = Aws.Util.option_bind (Aws.Xml.member "s3objectKey" xml) String.parse
    ; progress_percentage =
        Aws.Util.option_bind (Aws.Xml.member "progressPercentage" xml) Integer.parse
    ; store_task_state =
        Aws.Util.option_bind (Aws.Xml.member "storeTaskState" xml) String.parse
    ; store_task_failure_reason =
        Aws.Util.option_bind (Aws.Xml.member "storeTaskFailureReason" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.store_task_failure_reason (fun f ->
             Aws.Query.Pair ("StoreTaskFailureReason", String.to_query f))
       ; Aws.Util.option_map v.store_task_state (fun f ->
             Aws.Query.Pair ("StoreTaskState", String.to_query f))
       ; Aws.Util.option_map v.progress_percentage (fun f ->
             Aws.Query.Pair ("ProgressPercentage", Integer.to_query f))
       ; Aws.Util.option_map v.s3object_key (fun f ->
             Aws.Query.Pair ("S3objectKey", String.to_query f))
       ; Aws.Util.option_map v.bucket (fun f ->
             Aws.Query.Pair ("Bucket", String.to_query f))
       ; Aws.Util.option_map v.task_start_time (fun f ->
             Aws.Query.Pair ("TaskStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.ami_id (fun f ->
             Aws.Query.Pair ("AmiId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.store_task_failure_reason (fun f ->
             "storeTaskFailureReason", String.to_json f)
       ; Aws.Util.option_map v.store_task_state (fun f ->
             "storeTaskState", String.to_json f)
       ; Aws.Util.option_map v.progress_percentage (fun f ->
             "progressPercentage", Integer.to_json f)
       ; Aws.Util.option_map v.s3object_key (fun f -> "s3objectKey", String.to_json f)
       ; Aws.Util.option_map v.bucket (fun f -> "bucket", String.to_json f)
       ; Aws.Util.option_map v.task_start_time (fun f ->
             "taskStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.ami_id (fun f -> "amiId", String.to_json f)
       ])

let of_json j =
  { ami_id = Aws.Util.option_map (Aws.Json.lookup j "amiId") String.of_json
  ; task_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "taskStartTime") DateTime.of_json
  ; bucket = Aws.Util.option_map (Aws.Json.lookup j "bucket") String.of_json
  ; s3object_key = Aws.Util.option_map (Aws.Json.lookup j "s3objectKey") String.of_json
  ; progress_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "progressPercentage") Integer.of_json
  ; store_task_state =
      Aws.Util.option_map (Aws.Json.lookup j "storeTaskState") String.of_json
  ; store_task_failure_reason =
      Aws.Util.option_map (Aws.Json.lookup j "storeTaskFailureReason") String.of_json
  }
