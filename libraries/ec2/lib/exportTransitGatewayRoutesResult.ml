open Aws.BaseTypes

type t = { s3_location : String.t option }

let make ?s3_location () = { s3_location }

let parse xml =
  Some
    { s3_location = Aws.Util.option_bind (Aws.Xml.member "s3Location" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3_location (fun f ->
             Aws.Query.Pair ("S3Location", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s3_location (fun f -> "s3Location", String.to_json f) ])

let of_json j =
  { s3_location = Aws.Util.option_map (Aws.Json.lookup j "s3Location") String.of_json }
