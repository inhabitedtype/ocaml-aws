type t = ImageWatermark.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ImageWatermark.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageWatermark.to_query v
let to_json v = `List (List.map ImageWatermark.to_json v)
let of_json j = Aws.Json.to_list ImageWatermark.of_json j
