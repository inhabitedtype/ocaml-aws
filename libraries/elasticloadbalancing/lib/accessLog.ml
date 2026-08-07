open Aws.BaseTypes

type t =
  { enabled : Boolean.t
  ; s3_bucket_name : String.t option
  ; emit_interval : Integer.t option
  ; s3_bucket_prefix : String.t option
  }

let make ~enabled ?s3_bucket_name ?emit_interval ?s3_bucket_prefix () =
  { enabled; s3_bucket_name; emit_interval; s3_bucket_prefix }

let parse xml =
  Some
    { enabled =
        Aws.Xml.required
          "Enabled"
          (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse)
    ; s3_bucket_name =
        Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse
    ; emit_interval =
        Aws.Util.option_bind (Aws.Xml.member "EmitInterval" xml) Integer.parse
    ; s3_bucket_prefix =
        Aws.Util.option_bind (Aws.Xml.member "S3BucketPrefix" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3_bucket_prefix (fun f ->
             Aws.Query.Pair ("S3BucketPrefix", String.to_query f))
       ; Aws.Util.option_map v.emit_interval (fun f ->
             Aws.Query.Pair ("EmitInterval", Integer.to_query f))
       ; Aws.Util.option_map v.s3_bucket_name (fun f ->
             Aws.Query.Pair ("S3BucketName", String.to_query f))
       ; Some (Aws.Query.Pair ("Enabled", Boolean.to_query v.enabled))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3_bucket_prefix (fun f ->
             "S3BucketPrefix", String.to_json f)
       ; Aws.Util.option_map v.emit_interval (fun f -> "EmitInterval", Integer.to_json f)
       ; Aws.Util.option_map v.s3_bucket_name (fun f -> "S3BucketName", String.to_json f)
       ; Some ("Enabled", Boolean.to_json v.enabled)
       ])

let of_json j =
  { enabled = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Enabled"))
  ; s3_bucket_name = Aws.Util.option_map (Aws.Json.lookup j "S3BucketName") String.of_json
  ; emit_interval = Aws.Util.option_map (Aws.Json.lookup j "EmitInterval") Integer.of_json
  ; s3_bucket_prefix =
      Aws.Util.option_map (Aws.Json.lookup j "S3BucketPrefix") String.of_json
  }
