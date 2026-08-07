type t = FleetBlockDeviceMappingRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       FleetBlockDeviceMappingRequest.parse
       (Aws.Xml.members "BlockDeviceMapping" xml))

let to_query v = Aws.Query.to_query_list FleetBlockDeviceMappingRequest.to_query v
let to_json v = `List (List.map FleetBlockDeviceMappingRequest.to_json v)
let of_json j = Aws.Json.to_list FleetBlockDeviceMappingRequest.of_json j
