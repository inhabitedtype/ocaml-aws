type t = IpamOrganizationalUnitExclusion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamOrganizationalUnitExclusion.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamOrganizationalUnitExclusion.to_query v
let to_json v = `List (List.map IpamOrganizationalUnitExclusion.to_json v)
let of_json j = Aws.Json.to_list IpamOrganizationalUnitExclusion.of_json j
