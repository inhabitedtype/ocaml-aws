type t = VolumeStatusAction.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VolumeStatusAction.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VolumeStatusAction.to_query v
let to_json v = `List (List.map VolumeStatusAction.to_json v)
let of_json j = Aws.Json.to_list VolumeStatusAction.of_json j
