type t = Item.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Item.parse (Aws.Xml.members "Item" xml))
let to_query v = Aws.Query.to_query_list Item.to_query v
let to_json v = `List (List.map Item.to_json v)
let of_json j = Aws.Json.to_list Item.of_json j
