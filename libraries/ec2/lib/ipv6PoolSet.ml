type t = Ipv6Pool.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Ipv6Pool.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Ipv6Pool.to_query v
let to_json v = `List (List.map Ipv6Pool.to_json v)
let of_json j = Aws.Json.to_list Ipv6Pool.of_json j
