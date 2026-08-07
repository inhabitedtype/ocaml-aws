type t = ImageDiskContainer.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ImageDiskContainer.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageDiskContainer.to_query v
let to_json v = `List (List.map ImageDiskContainer.to_json v)
let of_json j = Aws.Json.to_list ImageDiskContainer.of_json j
