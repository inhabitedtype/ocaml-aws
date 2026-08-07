type t = IpamResourceTag.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpamResourceTag.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamResourceTag.to_query v
let to_json v = `List (List.map IpamResourceTag.to_json v)
let of_json j = Aws.Json.to_list IpamResourceTag.of_json j
