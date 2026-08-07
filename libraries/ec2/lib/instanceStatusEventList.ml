type t = InstanceStatusEvent.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceStatusEvent.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceStatusEvent.to_query v
let to_json v = `List (List.map InstanceStatusEvent.to_json v)
let of_json j = Aws.Json.to_list InstanceStatusEvent.of_json j
