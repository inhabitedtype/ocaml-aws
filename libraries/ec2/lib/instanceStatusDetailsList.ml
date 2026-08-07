type t = InstanceStatusDetails.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceStatusDetails.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceStatusDetails.to_query v
let to_json v = `List (List.map InstanceStatusDetails.to_json v)
let of_json j = Aws.Json.to_list InstanceStatusDetails.of_json j
