type t = Ipam.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Ipam.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Ipam.to_query v
let to_json v = `List (List.map Ipam.to_json v)
let of_json j = Aws.Json.to_list Ipam.of_json j
