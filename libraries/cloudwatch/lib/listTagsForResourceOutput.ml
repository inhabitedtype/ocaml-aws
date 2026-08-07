type t = { tags : TagList.t }

let make ?(tags = []) () = { tags }

let parse xml =
  Some
    { tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Tags", TagList.to_json v.tags) ])

let of_json j =
  { tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags")) }
