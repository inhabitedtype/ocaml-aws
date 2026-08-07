open Aws.BaseTypes

type t =
  { certificate_s3_bucket_name : String.t option
  ; certificate_s3_object_key : String.t option
  ; encryption_kms_key_id : String.t option
  }

let make ?certificate_s3_bucket_name ?certificate_s3_object_key ?encryption_kms_key_id ()
    =
  { certificate_s3_bucket_name; certificate_s3_object_key; encryption_kms_key_id }

let parse xml =
  Some
    { certificate_s3_bucket_name =
        Aws.Util.option_bind (Aws.Xml.member "certificateS3BucketName" xml) String.parse
    ; certificate_s3_object_key =
        Aws.Util.option_bind (Aws.Xml.member "certificateS3ObjectKey" xml) String.parse
    ; encryption_kms_key_id =
        Aws.Util.option_bind (Aws.Xml.member "encryptionKmsKeyId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.encryption_kms_key_id (fun f ->
             Aws.Query.Pair ("EncryptionKmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.certificate_s3_object_key (fun f ->
             Aws.Query.Pair ("CertificateS3ObjectKey", String.to_query f))
       ; Aws.Util.option_map v.certificate_s3_bucket_name (fun f ->
             Aws.Query.Pair ("CertificateS3BucketName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.encryption_kms_key_id (fun f ->
             "encryptionKmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.certificate_s3_object_key (fun f ->
             "certificateS3ObjectKey", String.to_json f)
       ; Aws.Util.option_map v.certificate_s3_bucket_name (fun f ->
             "certificateS3BucketName", String.to_json f)
       ])

let of_json j =
  { certificate_s3_bucket_name =
      Aws.Util.option_map (Aws.Json.lookup j "certificateS3BucketName") String.of_json
  ; certificate_s3_object_key =
      Aws.Util.option_map (Aws.Json.lookup j "certificateS3ObjectKey") String.of_json
  ; encryption_kms_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "encryptionKmsKeyId") String.of_json
  }
