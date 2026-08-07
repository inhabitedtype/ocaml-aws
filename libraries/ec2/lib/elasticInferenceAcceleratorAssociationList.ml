type t = ElasticInferenceAcceleratorAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ElasticInferenceAcceleratorAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ElasticInferenceAcceleratorAssociation.to_query v
let to_json v = `List (List.map ElasticInferenceAcceleratorAssociation.to_json v)
let of_json j = Aws.Json.to_list ElasticInferenceAcceleratorAssociation.of_json j
