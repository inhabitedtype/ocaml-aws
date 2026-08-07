type t = IpamOperatingRegion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpamOperatingRegion.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamOperatingRegion.to_query v
let to_json v = `List (List.map IpamOperatingRegion.to_json v)
let of_json j = Aws.Json.to_list IpamOperatingRegion.of_json j
