type t = RequestIpamResourceTag.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RequestIpamResourceTag.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RequestIpamResourceTag.to_query v
let to_json v = `List (List.map RequestIpamResourceTag.to_json v)
let of_json j = Aws.Json.to_list RequestIpamResourceTag.of_json j
