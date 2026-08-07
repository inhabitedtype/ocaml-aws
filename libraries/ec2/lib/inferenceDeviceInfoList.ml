type t = InferenceDeviceInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InferenceDeviceInfo.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InferenceDeviceInfo.to_query v
let to_json v = `List (List.map InferenceDeviceInfo.to_json v)
let of_json j = Aws.Json.to_list InferenceDeviceInfo.of_json j
