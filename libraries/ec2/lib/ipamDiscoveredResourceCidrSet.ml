type t = IpamDiscoveredResourceCidr.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamDiscoveredResourceCidr.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamDiscoveredResourceCidr.to_query v
let to_json v = `List (List.map IpamDiscoveredResourceCidr.to_json v)
let of_json j = Aws.Json.to_list IpamDiscoveredResourceCidr.of_json j
