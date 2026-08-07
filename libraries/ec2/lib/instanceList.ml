type t = Instance.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Instance.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Instance.to_query v
let to_json v = `List (List.map Instance.to_json v)
let of_json j = Aws.Json.to_list Instance.of_json j
