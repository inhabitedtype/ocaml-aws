type t = LaunchTemplateElasticInferenceAcceleratorResponse.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LaunchTemplateElasticInferenceAcceleratorResponse.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list LaunchTemplateElasticInferenceAcceleratorResponse.to_query v

let to_json v =
  `List (List.map LaunchTemplateElasticInferenceAcceleratorResponse.to_json v)

let of_json j =
  Aws.Json.to_list LaunchTemplateElasticInferenceAcceleratorResponse.of_json j
