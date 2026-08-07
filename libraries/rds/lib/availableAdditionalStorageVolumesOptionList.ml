type t = AvailableAdditionalStorageVolumesOption.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       AvailableAdditionalStorageVolumesOption.parse
       (Aws.Xml.members "AvailableAdditionalStorageVolumesOption" xml))

let to_query v =
  Aws.Query.to_query_list AvailableAdditionalStorageVolumesOption.to_query v

let to_json v = `List (List.map AvailableAdditionalStorageVolumesOption.to_json v)
let of_json j = Aws.Json.to_list AvailableAdditionalStorageVolumesOption.of_json j
