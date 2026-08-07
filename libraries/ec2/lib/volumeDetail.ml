open Aws.BaseTypes

type t = { size : Long.t }

let make ~size () = { size }

let parse xml =
  Some
    { size =
        Aws.Xml.required
          "size"
          (Aws.Util.option_bind (Aws.Xml.member "size" xml) Long.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Size", Long.to_query v.size)) ])

let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("size", Long.to_json v.size) ])

let of_json j =
  { size = Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "size")) }
