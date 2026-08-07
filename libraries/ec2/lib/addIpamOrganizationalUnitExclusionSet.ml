type t = AddIpamOrganizationalUnitExclusion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AddIpamOrganizationalUnitExclusion.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AddIpamOrganizationalUnitExclusion.to_query v
let to_json v = `List (List.map AddIpamOrganizationalUnitExclusion.to_json v)
let of_json j = Aws.Json.to_list AddIpamOrganizationalUnitExclusion.of_json j
