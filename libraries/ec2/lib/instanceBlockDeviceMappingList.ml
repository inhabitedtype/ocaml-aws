type t = InstanceBlockDeviceMapping.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InstanceBlockDeviceMapping.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceBlockDeviceMapping.to_query v
let to_json v = `List (List.map InstanceBlockDeviceMapping.to_json v)
let of_json j = Aws.Json.to_list InstanceBlockDeviceMapping.of_json j
