type t = SubnetConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SubnetConfiguration.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SubnetConfiguration.to_query v
let to_json v = `List (List.map SubnetConfiguration.to_json v)
let of_json j = Aws.Json.to_list SubnetConfiguration.of_json j
