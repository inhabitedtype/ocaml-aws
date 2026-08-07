type t = Trail.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Trail.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Trail.to_query v
let to_json v = `List (List.map Trail.to_json v)
let of_json j = Aws.Json.to_list Trail.of_json j
