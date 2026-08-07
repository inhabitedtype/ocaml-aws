type t = LocalStorageType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LocalStorageType.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LocalStorageType.to_query v
let to_json v = `List (List.map LocalStorageType.to_json v)
let of_json j = Aws.Json.to_list LocalStorageType.of_json j
