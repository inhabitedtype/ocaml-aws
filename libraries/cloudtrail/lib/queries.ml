type t = Query.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Query.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Query.to_query v
let to_json v = `List (List.map Query.to_json v)
let of_json j = Aws.Json.to_list Query.of_json j
