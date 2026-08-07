type t = AvailableProcessorFeature.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       AvailableProcessorFeature.parse
       (Aws.Xml.members "AvailableProcessorFeature" xml))

let to_query v = Aws.Query.to_query_list AvailableProcessorFeature.to_query v
let to_json v = `List (List.map AvailableProcessorFeature.to_json v)
let of_json j = Aws.Json.to_list AvailableProcessorFeature.of_json j
