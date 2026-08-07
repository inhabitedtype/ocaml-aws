type t = { tags : Tags.t }

let make ~tags () = { tags }

let parse xml =
  Some
    { tags =
        Aws.Xml.required
          "Tags"
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags)) ])

let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Tags", Tags.to_json v.tags) ])

let of_json j =
  { tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags")) }
