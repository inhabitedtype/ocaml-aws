open Aws.BaseTypes

type t = { public_type_arn : String.t option }

let make ?public_type_arn () = { public_type_arn }

let parse xml =
  Some
    { public_type_arn =
        Aws.Util.option_bind (Aws.Xml.member "PublicTypeArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_type_arn (fun f ->
             Aws.Query.Pair ("PublicTypeArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_type_arn (fun f ->
             "PublicTypeArn", String.to_json f)
       ])

let of_json j =
  { public_type_arn =
      Aws.Util.option_map (Aws.Json.lookup j "PublicTypeArn") String.of_json
  }
