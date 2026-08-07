open Aws.BaseTypes

type t =
  { image_id : String.t
  ; bucket : String.t
  ; s3_object_tags : S3ObjectTagList.t
  ; dry_run : Boolean.t option
  }

let make ~image_id ~bucket ?(s3_object_tags = []) ?dry_run () =
  { image_id; bucket; s3_object_tags; dry_run }

let parse xml =
  Some
    { image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; bucket =
        Aws.Xml.required
          "Bucket"
          (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml) String.parse)
    ; s3_object_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "S3ObjectTag" xml) S3ObjectTagList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("S3ObjectTag", S3ObjectTagList.to_query v.s3_object_tags))
       ; Some (Aws.Query.Pair ("Bucket", String.to_query v.bucket))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("S3ObjectTag", S3ObjectTagList.to_json v.s3_object_tags)
       ; Some ("Bucket", String.to_json v.bucket)
       ; Some ("ImageId", String.to_json v.image_id)
       ])

let of_json j =
  { image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; bucket = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket"))
  ; s3_object_tags =
      S3ObjectTagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3ObjectTag"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
