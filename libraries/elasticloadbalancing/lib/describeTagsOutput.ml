type t = { tag_descriptions : TagDescriptions.t }

let make ?(tag_descriptions = []) () = { tag_descriptions }

let parse xml =
  Some
    { tag_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagDescriptions" xml)
             TagDescriptions.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagDescriptions.member", TagDescriptions.to_query v.tag_descriptions))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagDescriptions", TagDescriptions.to_json v.tag_descriptions) ])

let of_json j =
  { tag_descriptions =
      TagDescriptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagDescriptions"))
  }
