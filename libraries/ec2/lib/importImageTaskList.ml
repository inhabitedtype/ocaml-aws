type t = ImportImageTask.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ImportImageTask.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImportImageTask.to_query v
let to_json v = `List (List.map ImportImageTask.to_json v)
let of_json j = Aws.Json.to_list ImportImageTask.of_json j
