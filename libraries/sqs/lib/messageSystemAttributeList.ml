type t = MessageSystemAttributeName.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map MessageSystemAttributeName.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MessageSystemAttributeName.to_query v
let to_json v = `List (List.map MessageSystemAttributeName.to_json v)
let of_json j = Aws.Json.to_list MessageSystemAttributeName.of_json j
