open Aws.BaseTypes

type t = { trail_name : String.t }

let make ~trail_name () = { trail_name }

let parse xml =
  Some
    { trail_name =
        Aws.Xml.required
          "TrailName"
          (Aws.Util.option_bind (Aws.Xml.member "TrailName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TrailName", String.to_query v.trail_name)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("TrailName", String.to_json v.trail_name) ])

let of_json j =
  { trail_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TrailName")) }
