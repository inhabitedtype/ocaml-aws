open Aws.BaseTypes

type t =
  { s3_location_uri : String.t
  ; s3_bucket_region : String.t
  ; s3_bucket_access_role_arn : String.t
  }

let make ~s3_location_uri ~s3_bucket_region ~s3_bucket_access_role_arn () =
  { s3_location_uri; s3_bucket_region; s3_bucket_access_role_arn }

let parse xml =
  Some
    { s3_location_uri =
        Aws.Xml.required
          "S3LocationUri"
          (Aws.Util.option_bind (Aws.Xml.member "S3LocationUri" xml) String.parse)
    ; s3_bucket_region =
        Aws.Xml.required
          "S3BucketRegion"
          (Aws.Util.option_bind (Aws.Xml.member "S3BucketRegion" xml) String.parse)
    ; s3_bucket_access_role_arn =
        Aws.Xml.required
          "S3BucketAccessRoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "S3BucketAccessRoleArn" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("S3BucketAccessRoleArn", String.to_query v.s3_bucket_access_role_arn))
       ; Some (Aws.Query.Pair ("S3BucketRegion", String.to_query v.s3_bucket_region))
       ; Some (Aws.Query.Pair ("S3LocationUri", String.to_query v.s3_location_uri))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("S3BucketAccessRoleArn", String.to_json v.s3_bucket_access_role_arn)
       ; Some ("S3BucketRegion", String.to_json v.s3_bucket_region)
       ; Some ("S3LocationUri", String.to_json v.s3_location_uri)
       ])

let of_json j =
  { s3_location_uri =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3LocationUri"))
  ; s3_bucket_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3BucketRegion"))
  ; s3_bucket_access_role_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3BucketAccessRoleArn"))
  }
