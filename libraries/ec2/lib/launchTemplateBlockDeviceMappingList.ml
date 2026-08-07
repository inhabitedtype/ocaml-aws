type t = LaunchTemplateBlockDeviceMapping.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LaunchTemplateBlockDeviceMapping.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LaunchTemplateBlockDeviceMapping.to_query v
let to_json v = `List (List.map LaunchTemplateBlockDeviceMapping.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplateBlockDeviceMapping.of_json j
