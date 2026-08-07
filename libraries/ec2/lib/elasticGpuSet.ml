type t = ElasticGpus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ElasticGpus.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ElasticGpus.to_query v
let to_json v = `List (List.map ElasticGpus.to_json v)
let of_json j = Aws.Json.to_list ElasticGpus.of_json j
