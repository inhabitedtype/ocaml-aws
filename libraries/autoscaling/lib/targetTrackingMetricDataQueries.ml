type t = TargetTrackingMetricDataQuery.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TargetTrackingMetricDataQuery.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TargetTrackingMetricDataQuery.to_query v
let to_json v = `List (List.map TargetTrackingMetricDataQuery.to_json v)
let of_json j = Aws.Json.to_list TargetTrackingMetricDataQuery.of_json j
