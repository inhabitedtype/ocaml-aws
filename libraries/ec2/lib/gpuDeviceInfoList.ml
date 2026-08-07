type t = GpuDeviceInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map GpuDeviceInfo.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list GpuDeviceInfo.to_query v
let to_json v = `List (List.map GpuDeviceInfo.to_json v)
let of_json j = Aws.Json.to_list GpuDeviceInfo.of_json j
