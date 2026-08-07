open Aws.BaseTypes

type t =
  { source_d_b_instance_arn : String.t
  ; backup_retention_period : Integer.t option
  ; kms_key_id : String.t option
  ; pre_signed_url : String.t option
  ; tags : TagList.t
  }

let make
    ~source_d_b_instance_arn
    ?backup_retention_period
    ?kms_key_id
    ?pre_signed_url
    ?(tags = [])
    () =
  { source_d_b_instance_arn; backup_retention_period; kms_key_id; pre_signed_url; tags }

let parse xml =
  Some
    { source_d_b_instance_arn =
        Aws.Xml.required
          "SourceDBInstanceArn"
          (Aws.Util.option_bind (Aws.Xml.member "SourceDBInstanceArn" xml) String.parse)
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; pre_signed_url =
        Aws.Util.option_bind (Aws.Xml.member "PreSignedUrl" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.pre_signed_url (fun f ->
             Aws.Query.Pair ("PreSignedUrl", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SourceDBInstanceArn", String.to_query v.source_d_b_instance_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.pre_signed_url (fun f -> "PreSignedUrl", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Some ("SourceDBInstanceArn", String.to_json v.source_d_b_instance_arn)
       ])

let of_json j =
  { source_d_b_instance_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceDBInstanceArn"))
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; pre_signed_url = Aws.Util.option_map (Aws.Json.lookup j "PreSignedUrl") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
