type t = Subnet.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Subnet.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Subnet.to_query v
let to_json v = `List (List.map Subnet.to_json v)
let of_json j = Aws.Json.to_list Subnet.of_json j
