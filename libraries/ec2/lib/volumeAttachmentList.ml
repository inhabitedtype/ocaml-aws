type t = VolumeAttachment.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VolumeAttachment.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VolumeAttachment.to_query v
let to_json v = `List (List.map VolumeAttachment.to_json v)
let of_json j = Aws.Json.to_list VolumeAttachment.of_json j
