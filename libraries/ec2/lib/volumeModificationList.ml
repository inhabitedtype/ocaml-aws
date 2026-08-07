type t = VolumeModification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VolumeModification.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VolumeModification.to_query v
let to_json v = `List (List.map VolumeModification.to_json v)
let of_json j = Aws.Json.to_list VolumeModification.of_json j
