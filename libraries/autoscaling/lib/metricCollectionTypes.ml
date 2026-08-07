type t = MetricCollectionType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MetricCollectionType.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MetricCollectionType.to_query v
let to_json v = `List (List.map MetricCollectionType.to_json v)
let of_json j = Aws.Json.to_list MetricCollectionType.of_json j
