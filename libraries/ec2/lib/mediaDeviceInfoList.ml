type t = MediaDeviceInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MediaDeviceInfo.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list MediaDeviceInfo.to_query v
let to_json v = `List (List.map MediaDeviceInfo.to_json v)
let of_json j = Aws.Json.to_list MediaDeviceInfo.of_json j
