type t = CreateVolumePermission.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CreateVolumePermission.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CreateVolumePermission.to_query v
let to_json v = `List (List.map CreateVolumePermission.to_json v)
let of_json j = Aws.Json.to_list CreateVolumePermission.of_json j
