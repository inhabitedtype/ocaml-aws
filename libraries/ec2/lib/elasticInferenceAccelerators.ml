type t = ElasticInferenceAccelerator.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ElasticInferenceAccelerator.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ElasticInferenceAccelerator.to_query v
let to_json v = `List (List.map ElasticInferenceAccelerator.to_json v)
let of_json j = Aws.Json.to_list ElasticInferenceAccelerator.of_json j
