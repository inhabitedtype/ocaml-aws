type t = AddIpamOperatingRegion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AddIpamOperatingRegion.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AddIpamOperatingRegion.to_query v
let to_json v = `List (List.map AddIpamOperatingRegion.to_json v)
let of_json j = Aws.Json.to_list AddIpamOperatingRegion.of_json j
