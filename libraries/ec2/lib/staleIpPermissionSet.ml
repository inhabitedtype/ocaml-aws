type t = StaleIpPermission.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StaleIpPermission.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list StaleIpPermission.to_query v
let to_json v = `List (List.map StaleIpPermission.to_json v)
let of_json j = Aws.Json.to_list StaleIpPermission.of_json j
