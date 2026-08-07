type t = Protocol.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Protocol.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Protocol.to_query v
let to_json v = `List (List.map Protocol.to_json v)
let of_json j = Aws.Json.to_list Protocol.of_json j
