type t = IpamScope.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpamScope.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamScope.to_query v
let to_json v = `List (List.map IpamScope.to_json v)
let of_json j = Aws.Json.to_list IpamScope.of_json j
