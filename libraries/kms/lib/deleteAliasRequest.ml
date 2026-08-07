open Aws.BaseTypes

type t = { alias_name : String.t }

let make ~alias_name () = { alias_name }

let parse xml =
  Some
    { alias_name =
        Aws.Xml.required
          "AliasName"
          (Aws.Util.option_bind (Aws.Xml.member "AliasName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("AliasName", String.to_query v.alias_name)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("AliasName", String.to_json v.alias_name) ])

let of_json j =
  { alias_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AliasName")) }
