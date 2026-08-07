type t = AccountAttributeName.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AccountAttributeName.parse (Aws.Xml.members "attributeName" xml))

let to_query v = Aws.Query.to_query_list AccountAttributeName.to_query v
let to_json v = `List (List.map AccountAttributeName.to_json v)
let of_json j = Aws.Json.to_list AccountAttributeName.of_json j
