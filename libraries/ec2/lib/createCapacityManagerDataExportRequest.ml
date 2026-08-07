open Aws.BaseTypes

type t =
  { s3_bucket_name : String.t
  ; s3_bucket_prefix : String.t option
  ; schedule : Schedule.t
  ; output_format : OutputFormat.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ~s3_bucket_name
    ?s3_bucket_prefix
    ~schedule
    ~output_format
    ?client_token
    ?dry_run
    ?(tag_specifications = [])
    () =
  { s3_bucket_name
  ; s3_bucket_prefix
  ; schedule
  ; output_format
  ; client_token
  ; dry_run
  ; tag_specifications
  }

let parse xml =
  Some
    { s3_bucket_name =
        Aws.Xml.required
          "S3BucketName"
          (Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse)
    ; s3_bucket_prefix =
        Aws.Util.option_bind (Aws.Xml.member "S3BucketPrefix" xml) String.parse
    ; schedule =
        Aws.Xml.required
          "Schedule"
          (Aws.Util.option_bind (Aws.Xml.member "Schedule" xml) Schedule.parse)
    ; output_format =
        Aws.Xml.required
          "OutputFormat"
          (Aws.Util.option_bind (Aws.Xml.member "OutputFormat" xml) OutputFormat.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some (Aws.Query.Pair ("OutputFormat", OutputFormat.to_query v.output_format))
       ; Some (Aws.Query.Pair ("Schedule", Schedule.to_query v.schedule))
       ; Aws.Util.option_map v.s3_bucket_prefix (fun f ->
             Aws.Query.Pair ("S3BucketPrefix", String.to_query f))
       ; Some (Aws.Query.Pair ("S3BucketName", String.to_query v.s3_bucket_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("OutputFormat", OutputFormat.to_json v.output_format)
       ; Some ("Schedule", Schedule.to_json v.schedule)
       ; Aws.Util.option_map v.s3_bucket_prefix (fun f ->
             "S3BucketPrefix", String.to_json f)
       ; Some ("S3BucketName", String.to_json v.s3_bucket_name)
       ])

let of_json j =
  { s3_bucket_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3BucketName"))
  ; s3_bucket_prefix =
      Aws.Util.option_map (Aws.Json.lookup j "S3BucketPrefix") String.of_json
  ; schedule = Schedule.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Schedule"))
  ; output_format =
      OutputFormat.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OutputFormat"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
