type t = UserIdGroupPair.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map UserIdGroupPair.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list UserIdGroupPair.to_query v
let to_json v = `List (List.map UserIdGroupPair.to_json v)
let of_json j = Aws.Json.to_list UserIdGroupPair.of_json j
