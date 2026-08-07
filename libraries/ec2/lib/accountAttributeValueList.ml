type t = AccountAttributeValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AccountAttributeValue.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AccountAttributeValue.to_query v
let to_json v = `List (List.map AccountAttributeValue.to_json v)
let of_json j = Aws.Json.to_list AccountAttributeValue.of_json j
