type t = AccountAttribute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AccountAttribute.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AccountAttribute.to_query v
let to_json v = `List (List.map AccountAttribute.to_json v)
let of_json j = Aws.Json.to_list AccountAttribute.of_json j
