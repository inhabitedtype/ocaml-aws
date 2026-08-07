type t = TargetNetwork.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TargetNetwork.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TargetNetwork.to_query v
let to_json v = `List (List.map TargetNetwork.to_json v)
let of_json j = Aws.Json.to_list TargetNetwork.of_json j
