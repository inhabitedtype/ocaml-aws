type t = IpamDiscoveredPublicAddress.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamDiscoveredPublicAddress.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamDiscoveredPublicAddress.to_query v
let to_json v = `List (List.map IpamDiscoveredPublicAddress.to_json v)
let of_json j = Aws.Json.to_list IpamDiscoveredPublicAddress.of_json j
