type t = SecurityGroupReference.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SecurityGroupReference.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SecurityGroupReference.to_query v
let to_json v = `List (List.map SecurityGroupReference.to_json v)
let of_json j = Aws.Json.to_list SecurityGroupReference.of_json j
