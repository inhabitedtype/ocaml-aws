type t = AdditionalStorageVolume.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AdditionalStorageVolume.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AdditionalStorageVolume.to_query v
let to_json v = `List (List.map AdditionalStorageVolume.to_json v)
let of_json j = Aws.Json.to_list AdditionalStorageVolume.of_json j
