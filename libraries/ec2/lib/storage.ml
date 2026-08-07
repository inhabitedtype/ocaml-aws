type t = { s3 : S3Storage.t option }

let make ?s3 () = { s3 }

let parse xml =
  Some { s3 = Aws.Util.option_bind (Aws.Xml.member "S3" xml) S3Storage.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3 (fun f -> Aws.Query.Pair ("S3", S3Storage.to_query f)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3 (fun f -> "S3", S3Storage.to_json f) ])

let of_json j = { s3 = Aws.Util.option_map (Aws.Json.lookup j "S3") S3Storage.of_json }
