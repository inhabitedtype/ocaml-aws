type t = UpgradeTarget.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map UpgradeTarget.parse (Aws.Xml.members "UpgradeTarget" xml))

let to_query v = Aws.Query.to_query_list UpgradeTarget.to_query v
let to_json v = `List (List.map UpgradeTarget.to_json v)
let of_json j = Aws.Json.to_list UpgradeTarget.of_json j
