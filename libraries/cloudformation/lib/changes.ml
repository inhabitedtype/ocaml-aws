type t = Change.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Change.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Change.to_query v
let to_json v = `List (List.map Change.to_json v)
let of_json j = Aws.Json.to_list Change.of_json j
