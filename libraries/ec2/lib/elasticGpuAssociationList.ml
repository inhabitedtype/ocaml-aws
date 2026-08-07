type t = ElasticGpuAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ElasticGpuAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ElasticGpuAssociation.to_query v
let to_json v = `List (List.map ElasticGpuAssociation.to_json v)
let of_json j = Aws.Json.to_list ElasticGpuAssociation.of_json j
