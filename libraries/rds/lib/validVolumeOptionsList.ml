type t = ValidVolumeOptions.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ValidVolumeOptions.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ValidVolumeOptions.to_query v
let to_json v = `List (List.map ValidVolumeOptions.to_json v)
let of_json j = Aws.Json.to_list ValidVolumeOptions.of_json j
