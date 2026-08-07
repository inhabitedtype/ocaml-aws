type t = VolumeStatusItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VolumeStatusItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VolumeStatusItem.to_query v
let to_json v = `List (List.map VolumeStatusItem.to_json v)
let of_json j = Aws.Json.to_list VolumeStatusItem.of_json j
