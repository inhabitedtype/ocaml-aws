open Aws.BaseTypes

type t = { import_id : String.t }

let make ~import_id () = { import_id }

let parse xml =
  Some
    { import_id =
        Aws.Xml.required
          "ImportId"
          (Aws.Util.option_bind (Aws.Xml.member "ImportId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ImportId", String.to_query v.import_id)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("ImportId", String.to_json v.import_id) ])

let of_json j =
  { import_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImportId")) }
