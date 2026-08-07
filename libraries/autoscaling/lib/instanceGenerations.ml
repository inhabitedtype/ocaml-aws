type t = InstanceGeneration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceGeneration.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InstanceGeneration.to_query v
let to_json v = `List (List.map InstanceGeneration.to_json v)
let of_json j = Aws.Json.to_list InstanceGeneration.of_json j
