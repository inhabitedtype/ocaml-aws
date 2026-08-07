type t = VolumeStatusAttachmentStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map VolumeStatusAttachmentStatus.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VolumeStatusAttachmentStatus.to_query v
let to_json v = `List (List.map VolumeStatusAttachmentStatus.to_json v)
let of_json j = Aws.Json.to_list VolumeStatusAttachmentStatus.of_json j
