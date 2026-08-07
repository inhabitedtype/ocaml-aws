type t = LaunchTemplateBlockDeviceMappingRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LaunchTemplateBlockDeviceMappingRequest.parse
       (Aws.Xml.members "BlockDeviceMapping" xml))

let to_query v =
  Aws.Query.to_query_list LaunchTemplateBlockDeviceMappingRequest.to_query v

let to_json v = `List (List.map LaunchTemplateBlockDeviceMappingRequest.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplateBlockDeviceMappingRequest.of_json j
