type t = { attributes : AttributeList.t }

let make ?(attributes = []) () = { attributes }
let parse xml = Some { attributes = Aws.Util.of_option [] (AttributeList.parse xml) }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Attributes.member", AttributeList.to_query v.attributes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("Attributes", AttributeList.to_json v.attributes) ])

let of_json j =
  { attributes =
      AttributeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attributes"))
  }
