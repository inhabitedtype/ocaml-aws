type t = Image.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Image.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Image.to_query v
let to_json v = `List (List.map Image.to_json v)
let of_json j = Aws.Json.to_list Image.of_json j
