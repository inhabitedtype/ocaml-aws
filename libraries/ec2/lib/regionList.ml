type t = Region.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Region.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Region.to_query v
let to_json v = `List (List.map Region.to_json v)
let of_json j = Aws.Json.to_list Region.of_json j
