type t = AdditionalStorageVolumeOutput.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AdditionalStorageVolumeOutput.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AdditionalStorageVolumeOutput.to_query v
let to_json v = `List (List.map AdditionalStorageVolumeOutput.to_json v)
let of_json j = Aws.Json.to_list AdditionalStorageVolumeOutput.of_json j
