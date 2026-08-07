type t = { tag_list : TagList.t }

let make ?(tag_list = []) () = { tag_list }

let parse xml =
  Some
    { tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("TagList", TagList.to_json v.tag_list) ])

let of_json j =
  { tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList")) }
