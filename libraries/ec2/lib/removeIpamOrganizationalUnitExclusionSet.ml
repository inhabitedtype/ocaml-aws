type t = RemoveIpamOrganizationalUnitExclusion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map RemoveIpamOrganizationalUnitExclusion.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list RemoveIpamOrganizationalUnitExclusion.to_query v
let to_json v = `List (List.map RemoveIpamOrganizationalUnitExclusion.to_json v)
let of_json j = Aws.Json.to_list RemoveIpamOrganizationalUnitExclusion.of_json j
