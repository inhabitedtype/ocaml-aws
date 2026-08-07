type t = Attribute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Attribute.parse (Aws.Xml.members "Attribute" xml))

let to_query v = Aws.Query.to_query_list Attribute.to_query v
let to_json v = `List (List.map Attribute.to_json v)
let of_json j = Aws.Json.to_list Attribute.of_json j
