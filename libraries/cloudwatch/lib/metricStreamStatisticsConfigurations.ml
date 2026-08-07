type t = MetricStreamStatisticsConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map MetricStreamStatisticsConfiguration.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MetricStreamStatisticsConfiguration.to_query v
let to_json v = `List (List.map MetricStreamStatisticsConfiguration.to_json v)
let of_json j = Aws.Json.to_list MetricStreamStatisticsConfiguration.of_json j
