type t = ElasticGpuSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       ElasticGpuSpecification.parse
       (Aws.Xml.members "ElasticGpuSpecification" xml))

let to_query v = Aws.Query.to_query_list ElasticGpuSpecification.to_query v
let to_json v = `List (List.map ElasticGpuSpecification.to_json v)
let of_json j = Aws.Json.to_list ElasticGpuSpecification.of_json j
