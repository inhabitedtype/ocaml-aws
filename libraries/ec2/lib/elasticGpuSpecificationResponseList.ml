type t = ElasticGpuSpecificationResponse.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ElasticGpuSpecificationResponse.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ElasticGpuSpecificationResponse.to_query v
let to_json v = `List (List.map ElasticGpuSpecificationResponse.to_json v)
let of_json j = Aws.Json.to_list ElasticGpuSpecificationResponse.of_json j
