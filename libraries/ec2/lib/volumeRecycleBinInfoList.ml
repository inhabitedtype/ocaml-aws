type t = VolumeRecycleBinInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VolumeRecycleBinInfo.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VolumeRecycleBinInfo.to_query v
let to_json v = `List (List.map VolumeRecycleBinInfo.to_json v)
let of_json j = Aws.Json.to_list VolumeRecycleBinInfo.of_json j
