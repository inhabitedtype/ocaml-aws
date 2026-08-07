type t = ImportInstanceVolumeDetailItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ImportInstanceVolumeDetailItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImportInstanceVolumeDetailItem.to_query v
let to_json v = `List (List.map ImportInstanceVolumeDetailItem.to_json v)
let of_json j = Aws.Json.to_list ImportInstanceVolumeDetailItem.of_json j
