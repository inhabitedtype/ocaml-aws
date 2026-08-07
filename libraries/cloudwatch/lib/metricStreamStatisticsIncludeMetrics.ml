type t = MetricStreamStatisticsMetric.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map MetricStreamStatisticsMetric.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MetricStreamStatisticsMetric.to_query v
let to_json v = `List (List.map MetricStreamStatisticsMetric.to_json v)
let of_json j = Aws.Json.to_list MetricStreamStatisticsMetric.of_json j
