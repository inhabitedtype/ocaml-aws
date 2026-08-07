type t = IpPermission.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpPermission.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpPermission.to_query v
let to_json v = `List (List.map IpPermission.to_json v)
let of_json j = Aws.Json.to_list IpPermission.of_json j
