type t = Tag.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "Tag" xml))
let to_query v = Aws.Query.to_query_list Tag.to_query v
let to_json v = `List (List.map Tag.to_json v)
let of_json j = Aws.Json.to_list Tag.of_json j
