type t = ProcessorFeature.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ProcessorFeature.parse (Aws.Xml.members "ProcessorFeature" xml))

let to_query v = Aws.Query.to_query_list ProcessorFeature.to_query v
let to_json v = `List (List.map ProcessorFeature.to_json v)
let of_json j = Aws.Json.to_list ProcessorFeature.of_json j
