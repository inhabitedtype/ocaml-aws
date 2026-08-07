type t = VolumeStatusEvent.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VolumeStatusEvent.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VolumeStatusEvent.to_query v
let to_json v = `List (List.map VolumeStatusEvent.to_json v)
let of_json j = Aws.Json.to_list VolumeStatusEvent.of_json j
