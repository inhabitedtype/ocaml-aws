type t = ImageWatermarkFilterResponse.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ImageWatermarkFilterResponse.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageWatermarkFilterResponse.to_query v
let to_json v = `List (List.map ImageWatermarkFilterResponse.to_json v)
let of_json j = Aws.Json.to_list ImageWatermarkFilterResponse.of_json j
