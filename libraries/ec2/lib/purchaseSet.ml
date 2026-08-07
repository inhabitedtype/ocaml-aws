type t = Purchase.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Purchase.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Purchase.to_query v
let to_json v = `List (List.map Purchase.to_json v)
let of_json j = Aws.Json.to_list Purchase.of_json j
