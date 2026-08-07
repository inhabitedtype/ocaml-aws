type t = ReplaceableAttribute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ReplaceableAttribute.parse (Aws.Xml.members "Attribute" xml))

let to_query v = Aws.Query.to_query_list ReplaceableAttribute.to_query v
let to_json v = `List (List.map ReplaceableAttribute.to_json v)
let of_json j = Aws.Json.to_list ReplaceableAttribute.of_json j
