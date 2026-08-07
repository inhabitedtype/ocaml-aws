type t = BlockDeviceMapping.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map BlockDeviceMapping.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list BlockDeviceMapping.to_query v
let to_json v = `List (List.map BlockDeviceMapping.to_json v)
let of_json j = Aws.Json.to_list BlockDeviceMapping.of_json j
