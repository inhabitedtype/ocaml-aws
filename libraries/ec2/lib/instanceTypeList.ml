type t = InstanceType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceType.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InstanceType.to_query v
let to_json v = `List (List.map InstanceType.to_json v)
let of_json j = Aws.Json.to_list InstanceType.of_json j
