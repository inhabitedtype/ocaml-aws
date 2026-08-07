type t = Filter.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Filter.parse (Aws.Xml.members "Filter" xml))
let to_query v = Aws.Query.to_query_list Filter.to_query v
let to_json v = `List (List.map Filter.to_json v)
let of_json j = Aws.Json.to_list Filter.of_json j
