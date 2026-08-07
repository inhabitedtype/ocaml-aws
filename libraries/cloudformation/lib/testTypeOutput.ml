open Aws.BaseTypes

type t = { type_version_arn : String.t option }

let make ?type_version_arn () = { type_version_arn }

let parse xml =
  Some
    { type_version_arn =
        Aws.Util.option_bind (Aws.Xml.member "TypeVersionArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_version_arn (fun f ->
             Aws.Query.Pair ("TypeVersionArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_version_arn (fun f ->
             "TypeVersionArn", String.to_json f)
       ])

let of_json j =
  { type_version_arn =
      Aws.Util.option_map (Aws.Json.lookup j "TypeVersionArn") String.of_json
  }
