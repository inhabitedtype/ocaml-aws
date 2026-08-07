open Aws.BaseTypes

type t =
  { export_task_identifier : String.t
  ; source_arn : String.t
  ; s3_bucket_name : String.t
  ; iam_role_arn : String.t
  ; kms_key_id : String.t
  ; s3_prefix : String.t option
  ; export_only : StringList.t
  }

let make
    ~export_task_identifier
    ~source_arn
    ~s3_bucket_name
    ~iam_role_arn
    ~kms_key_id
    ?s3_prefix
    ?(export_only = [])
    () =
  { export_task_identifier
  ; source_arn
  ; s3_bucket_name
  ; iam_role_arn
  ; kms_key_id
  ; s3_prefix
  ; export_only
  }

let parse xml =
  Some
    { export_task_identifier =
        Aws.Xml.required
          "ExportTaskIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "ExportTaskIdentifier" xml) String.parse)
    ; source_arn =
        Aws.Xml.required
          "SourceArn"
          (Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse)
    ; s3_bucket_name =
        Aws.Xml.required
          "S3BucketName"
          (Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse)
    ; iam_role_arn =
        Aws.Xml.required
          "IamRoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "IamRoleArn" xml) String.parse)
    ; kms_key_id =
        Aws.Xml.required
          "KmsKeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse)
    ; s3_prefix = Aws.Util.option_bind (Aws.Xml.member "S3Prefix" xml) String.parse
    ; export_only =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ExportOnly" xml) StringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ExportOnly.member", StringList.to_query v.export_only))
       ; Aws.Util.option_map v.s3_prefix (fun f ->
             Aws.Query.Pair ("S3Prefix", String.to_query f))
       ; Some (Aws.Query.Pair ("KmsKeyId", String.to_query v.kms_key_id))
       ; Some (Aws.Query.Pair ("IamRoleArn", String.to_query v.iam_role_arn))
       ; Some (Aws.Query.Pair ("S3BucketName", String.to_query v.s3_bucket_name))
       ; Some (Aws.Query.Pair ("SourceArn", String.to_query v.source_arn))
       ; Some
           (Aws.Query.Pair
              ("ExportTaskIdentifier", String.to_query v.export_task_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ExportOnly", StringList.to_json v.export_only)
       ; Aws.Util.option_map v.s3_prefix (fun f -> "S3Prefix", String.to_json f)
       ; Some ("KmsKeyId", String.to_json v.kms_key_id)
       ; Some ("IamRoleArn", String.to_json v.iam_role_arn)
       ; Some ("S3BucketName", String.to_json v.s3_bucket_name)
       ; Some ("SourceArn", String.to_json v.source_arn)
       ; Some ("ExportTaskIdentifier", String.to_json v.export_task_identifier)
       ])

let of_json j =
  { export_task_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExportTaskIdentifier"))
  ; source_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceArn"))
  ; s3_bucket_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3BucketName"))
  ; iam_role_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IamRoleArn"))
  ; kms_key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KmsKeyId"))
  ; s3_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3Prefix") String.of_json
  ; export_only =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExportOnly"))
  }
