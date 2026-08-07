type t = IpamPoolAllocation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpamPoolAllocation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPoolAllocation.to_query v
let to_json v = `List (List.map IpamPoolAllocation.to_json v)
let of_json j = Aws.Json.to_list IpamPoolAllocation.of_json j
