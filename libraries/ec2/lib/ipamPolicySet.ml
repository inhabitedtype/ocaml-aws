type t = IpamPolicy.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpamPolicy.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPolicy.to_query v
let to_json v = `List (List.map IpamPolicy.to_json v)
let of_json j = Aws.Json.to_list IpamPolicy.of_json j
