open Aws.BaseTypes

type t = { enabled : Boolean.t }

let make ~enabled () = { enabled }

let parse xml =
  Some
    { enabled =
        Aws.Xml.required
          "Enabled"
          (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Enabled", Boolean.to_query v.enabled)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Enabled", Boolean.to_json v.enabled) ])

let of_json j =
  { enabled = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Enabled")) }
