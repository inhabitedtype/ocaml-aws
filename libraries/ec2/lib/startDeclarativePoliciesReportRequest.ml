open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; s3_bucket : String.t
  ; s3_prefix : String.t option
  ; target_id : String.t
  ; tag_specifications : TagSpecificationList.t
  }

let make ?dry_run ~s3_bucket ?s3_prefix ~target_id ?(tag_specifications = []) () =
  { dry_run; s3_bucket; s3_prefix; target_id; tag_specifications }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; s3_bucket =
        Aws.Xml.required
          "S3Bucket"
          (Aws.Util.option_bind (Aws.Xml.member "S3Bucket" xml) String.parse)
    ; s3_prefix = Aws.Util.option_bind (Aws.Xml.member "S3Prefix" xml) String.parse
    ; target_id =
        Aws.Xml.required
          "TargetId"
          (Aws.Util.option_bind (Aws.Xml.member "TargetId" xml) String.parse)
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
       ; Some (Aws.Query.Pair ("TargetId", String.to_query v.target_id))
       ; Aws.Util.option_map v.s3_prefix (fun f ->
             Aws.Query.Pair ("S3Prefix", String.to_query f))
       ; Some (Aws.Query.Pair ("S3Bucket", String.to_query v.s3_bucket))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("TargetId", String.to_json v.target_id)
       ; Aws.Util.option_map v.s3_prefix (fun f -> "S3Prefix", String.to_json f)
       ; Some ("S3Bucket", String.to_json v.s3_bucket)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; s3_bucket = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3Bucket"))
  ; s3_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3Prefix") String.of_json
  ; target_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetId"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
