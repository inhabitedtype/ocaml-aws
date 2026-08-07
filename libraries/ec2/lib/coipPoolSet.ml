type t = CoipPool.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map CoipPool.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list CoipPool.to_query v
let to_json v = `List (List.map CoipPool.to_json v)
let of_json j = Aws.Json.to_list CoipPool.of_json j
