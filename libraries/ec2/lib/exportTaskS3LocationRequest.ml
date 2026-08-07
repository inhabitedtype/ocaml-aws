open Aws.BaseTypes

type t =
  { s3_bucket : String.t
  ; s3_prefix : String.t option
  }

let make ~s3_bucket ?s3_prefix () = { s3_bucket; s3_prefix }

let parse xml =
  Some
    { s3_bucket =
        Aws.Xml.required
          "S3Bucket"
          (Aws.Util.option_bind (Aws.Xml.member "S3Bucket" xml) String.parse)
    ; s3_prefix = Aws.Util.option_bind (Aws.Xml.member "S3Prefix" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3_prefix (fun f ->
             Aws.Query.Pair ("S3Prefix", String.to_query f))
       ; Some (Aws.Query.Pair ("S3Bucket", String.to_query v.s3_bucket))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3_prefix (fun f -> "S3Prefix", String.to_json f)
       ; Some ("S3Bucket", String.to_json v.s3_bucket)
       ])

let of_json j =
  { s3_bucket = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3Bucket"))
  ; s3_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3Prefix") String.of_json
  }
