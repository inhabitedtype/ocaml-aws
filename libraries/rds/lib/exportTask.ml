open Aws.BaseTypes

type t =
  { export_task_identifier : String.t option
  ; source_arn : String.t option
  ; export_only : StringList.t
  ; snapshot_time : DateTime.t option
  ; task_start_time : DateTime.t option
  ; task_end_time : DateTime.t option
  ; s3_bucket : String.t option
  ; s3_prefix : String.t option
  ; iam_role_arn : String.t option
  ; kms_key_id : String.t option
  ; status : String.t option
  ; percent_progress : Integer.t option
  ; total_extracted_data_in_g_b : Integer.t option
  ; failure_cause : String.t option
  ; warning_message : String.t option
  ; source_type : ExportSourceType.t option
  }

let make
    ?export_task_identifier
    ?source_arn
    ?(export_only = [])
    ?snapshot_time
    ?task_start_time
    ?task_end_time
    ?s3_bucket
    ?s3_prefix
    ?iam_role_arn
    ?kms_key_id
    ?status
    ?percent_progress
    ?total_extracted_data_in_g_b
    ?failure_cause
    ?warning_message
    ?source_type
    () =
  { export_task_identifier
  ; source_arn
  ; export_only
  ; snapshot_time
  ; task_start_time
  ; task_end_time
  ; s3_bucket
  ; s3_prefix
  ; iam_role_arn
  ; kms_key_id
  ; status
  ; percent_progress
  ; total_extracted_data_in_g_b
  ; failure_cause
  ; warning_message
  ; source_type
  }

let parse xml =
  Some
    { export_task_identifier =
        Aws.Util.option_bind (Aws.Xml.member "ExportTaskIdentifier" xml) String.parse
    ; source_arn = Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse
    ; export_only =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ExportOnly" xml) StringList.parse)
    ; snapshot_time =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotTime" xml) DateTime.parse
    ; task_start_time =
        Aws.Util.option_bind (Aws.Xml.member "TaskStartTime" xml) DateTime.parse
    ; task_end_time =
        Aws.Util.option_bind (Aws.Xml.member "TaskEndTime" xml) DateTime.parse
    ; s3_bucket = Aws.Util.option_bind (Aws.Xml.member "S3Bucket" xml) String.parse
    ; s3_prefix = Aws.Util.option_bind (Aws.Xml.member "S3Prefix" xml) String.parse
    ; iam_role_arn = Aws.Util.option_bind (Aws.Xml.member "IamRoleArn" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; percent_progress =
        Aws.Util.option_bind (Aws.Xml.member "PercentProgress" xml) Integer.parse
    ; total_extracted_data_in_g_b =
        Aws.Util.option_bind (Aws.Xml.member "TotalExtractedDataInGB" xml) Integer.parse
    ; failure_cause =
        Aws.Util.option_bind (Aws.Xml.member "FailureCause" xml) String.parse
    ; warning_message =
        Aws.Util.option_bind (Aws.Xml.member "WarningMessage" xml) String.parse
    ; source_type =
        Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) ExportSourceType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_type (fun f ->
             Aws.Query.Pair ("SourceType", ExportSourceType.to_query f))
       ; Aws.Util.option_map v.warning_message (fun f ->
             Aws.Query.Pair ("WarningMessage", String.to_query f))
       ; Aws.Util.option_map v.failure_cause (fun f ->
             Aws.Query.Pair ("FailureCause", String.to_query f))
       ; Aws.Util.option_map v.total_extracted_data_in_g_b (fun f ->
             Aws.Query.Pair ("TotalExtractedDataInGB", Integer.to_query f))
       ; Aws.Util.option_map v.percent_progress (fun f ->
             Aws.Query.Pair ("PercentProgress", Integer.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.iam_role_arn (fun f ->
             Aws.Query.Pair ("IamRoleArn", String.to_query f))
       ; Aws.Util.option_map v.s3_prefix (fun f ->
             Aws.Query.Pair ("S3Prefix", String.to_query f))
       ; Aws.Util.option_map v.s3_bucket (fun f ->
             Aws.Query.Pair ("S3Bucket", String.to_query f))
       ; Aws.Util.option_map v.task_end_time (fun f ->
             Aws.Query.Pair ("TaskEndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.task_start_time (fun f ->
             Aws.Query.Pair ("TaskStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.snapshot_time (fun f ->
             Aws.Query.Pair ("SnapshotTime", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("ExportOnly.member", StringList.to_query v.export_only))
       ; Aws.Util.option_map v.source_arn (fun f ->
             Aws.Query.Pair ("SourceArn", String.to_query f))
       ; Aws.Util.option_map v.export_task_identifier (fun f ->
             Aws.Query.Pair ("ExportTaskIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_type (fun f ->
             "SourceType", ExportSourceType.to_json f)
       ; Aws.Util.option_map v.warning_message (fun f ->
             "WarningMessage", String.to_json f)
       ; Aws.Util.option_map v.failure_cause (fun f -> "FailureCause", String.to_json f)
       ; Aws.Util.option_map v.total_extracted_data_in_g_b (fun f ->
             "TotalExtractedDataInGB", Integer.to_json f)
       ; Aws.Util.option_map v.percent_progress (fun f ->
             "PercentProgress", Integer.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.iam_role_arn (fun f -> "IamRoleArn", String.to_json f)
       ; Aws.Util.option_map v.s3_prefix (fun f -> "S3Prefix", String.to_json f)
       ; Aws.Util.option_map v.s3_bucket (fun f -> "S3Bucket", String.to_json f)
       ; Aws.Util.option_map v.task_end_time (fun f -> "TaskEndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.task_start_time (fun f ->
             "TaskStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.snapshot_time (fun f -> "SnapshotTime", DateTime.to_json f)
       ; Some ("ExportOnly", StringList.to_json v.export_only)
       ; Aws.Util.option_map v.source_arn (fun f -> "SourceArn", String.to_json f)
       ; Aws.Util.option_map v.export_task_identifier (fun f ->
             "ExportTaskIdentifier", String.to_json f)
       ])

let of_json j =
  { export_task_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "ExportTaskIdentifier") String.of_json
  ; source_arn = Aws.Util.option_map (Aws.Json.lookup j "SourceArn") String.of_json
  ; export_only =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExportOnly"))
  ; snapshot_time =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotTime") DateTime.of_json
  ; task_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "TaskStartTime") DateTime.of_json
  ; task_end_time = Aws.Util.option_map (Aws.Json.lookup j "TaskEndTime") DateTime.of_json
  ; s3_bucket = Aws.Util.option_map (Aws.Json.lookup j "S3Bucket") String.of_json
  ; s3_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3Prefix") String.of_json
  ; iam_role_arn = Aws.Util.option_map (Aws.Json.lookup j "IamRoleArn") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; percent_progress =
      Aws.Util.option_map (Aws.Json.lookup j "PercentProgress") Integer.of_json
  ; total_extracted_data_in_g_b =
      Aws.Util.option_map (Aws.Json.lookup j "TotalExtractedDataInGB") Integer.of_json
  ; failure_cause = Aws.Util.option_map (Aws.Json.lookup j "FailureCause") String.of_json
  ; warning_message =
      Aws.Util.option_map (Aws.Json.lookup j "WarningMessage") String.of_json
  ; source_type =
      Aws.Util.option_map (Aws.Json.lookup j "SourceType") ExportSourceType.of_json
  }
