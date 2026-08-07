type t = Export.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Export.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Export.to_query v
let to_json v = `List (List.map Export.to_json v)
let of_json j = Aws.Json.to_list Export.of_json j
