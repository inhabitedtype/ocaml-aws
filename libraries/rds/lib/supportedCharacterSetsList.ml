type t = CharacterSet.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CharacterSet.parse (Aws.Xml.members "CharacterSet" xml))

let to_query v = Aws.Query.to_query_list CharacterSet.to_query v
let to_json v = `List (List.map CharacterSet.to_json v)
let of_json j = Aws.Json.to_list CharacterSet.of_json j
