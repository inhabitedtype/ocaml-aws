type t = AttributeValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AttributeValue.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AttributeValue.to_query v
let to_json v = `List (List.map AttributeValue.to_json v)
let of_json j = Aws.Json.to_list AttributeValue.of_json j
