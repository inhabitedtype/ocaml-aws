type t = IpamPool.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map IpamPool.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list IpamPool.to_query v
let to_json v = `List (List.map IpamPool.to_json v)
let of_json j = Aws.Json.to_list IpamPool.of_json j
