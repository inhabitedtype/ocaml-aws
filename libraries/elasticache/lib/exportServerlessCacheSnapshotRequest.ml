open Aws.BaseTypes

type t =
  { serverless_cache_snapshot_name : String.t
  ; s3_bucket_name : String.t
  }

let make ~serverless_cache_snapshot_name ~s3_bucket_name () =
  { serverless_cache_snapshot_name; s3_bucket_name }

let parse xml =
  Some
    { serverless_cache_snapshot_name =
        Aws.Xml.required
          "ServerlessCacheSnapshotName"
          (Aws.Util.option_bind
             (Aws.Xml.member "ServerlessCacheSnapshotName" xml)
             String.parse)
    ; s3_bucket_name =
        Aws.Xml.required
          "S3BucketName"
          (Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("S3BucketName", String.to_query v.s3_bucket_name))
       ; Some
           (Aws.Query.Pair
              ( "ServerlessCacheSnapshotName"
              , String.to_query v.serverless_cache_snapshot_name ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("S3BucketName", String.to_json v.s3_bucket_name)
       ; Some
           ("ServerlessCacheSnapshotName", String.to_json v.serverless_cache_snapshot_name)
       ])

let of_json j =
  { serverless_cache_snapshot_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessCacheSnapshotName"))
  ; s3_bucket_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3BucketName"))
  }
