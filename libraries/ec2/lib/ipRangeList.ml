type t = IpRange.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map IpRange.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list IpRange.to_query v
let to_json v = `List (List.map IpRange.to_json v)
let of_json j = Aws.Json.to_list IpRange.of_json j
