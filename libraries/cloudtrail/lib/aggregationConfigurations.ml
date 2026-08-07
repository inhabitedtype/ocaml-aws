type t = AggregationConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AggregationConfiguration.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AggregationConfiguration.to_query v
let to_json v = `List (List.map AggregationConfiguration.to_json v)
let of_json j = Aws.Json.to_list AggregationConfiguration.of_json j
