type t = PolicyDescriptorType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PolicyDescriptorType.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list PolicyDescriptorType.to_query v
let to_json v = `List (List.map PolicyDescriptorType.to_json v)
let of_json j = Aws.Json.to_list PolicyDescriptorType.of_json j
