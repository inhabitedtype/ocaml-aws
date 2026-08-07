type t = TagDescription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TagDescription.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TagDescription.to_query v
let to_json v = `List (List.map TagDescription.to_json v)
let of_json j = Aws.Json.to_list TagDescription.of_json j
