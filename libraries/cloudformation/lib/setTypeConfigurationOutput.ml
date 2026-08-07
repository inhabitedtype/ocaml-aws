open Aws.BaseTypes

type t = { configuration_arn : String.t option }

let make ?configuration_arn () = { configuration_arn }

let parse xml =
  Some
    { configuration_arn =
        Aws.Util.option_bind (Aws.Xml.member "ConfigurationArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configuration_arn (fun f ->
             Aws.Query.Pair ("ConfigurationArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configuration_arn (fun f ->
             "ConfigurationArn", String.to_json f)
       ])

let of_json j =
  { configuration_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ConfigurationArn") String.of_json
  }
