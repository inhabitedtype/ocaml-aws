type t = { s3 : S3ImportSource.t }

let make ~s3 () = { s3 }

let parse xml =
  Some
    { s3 =
        Aws.Xml.required
          "S3"
          (Aws.Util.option_bind (Aws.Xml.member "S3" xml) S3ImportSource.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("S3", S3ImportSource.to_query v.s3)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("S3", S3ImportSource.to_json v.s3) ])

let of_json j =
  { s3 = S3ImportSource.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3")) }
