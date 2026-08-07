type t = ImageWatermarkFilterRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ImageWatermarkFilterRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageWatermarkFilterRequest.to_query v
let to_json v = `List (List.map ImageWatermarkFilterRequest.to_json v)
let of_json j = Aws.Json.to_list ImageWatermarkFilterRequest.of_json j
