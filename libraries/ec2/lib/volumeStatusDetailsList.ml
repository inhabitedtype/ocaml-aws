type t = VolumeStatusDetails.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VolumeStatusDetails.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VolumeStatusDetails.to_query v
let to_json v = `List (List.map VolumeStatusDetails.to_json v)
let of_json j = Aws.Json.to_list VolumeStatusDetails.of_json j
