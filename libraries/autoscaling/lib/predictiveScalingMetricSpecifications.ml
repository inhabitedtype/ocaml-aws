type t = PredictiveScalingMetricSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map PredictiveScalingMetricSpecification.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list PredictiveScalingMetricSpecification.to_query v
let to_json v = `List (List.map PredictiveScalingMetricSpecification.to_json v)
let of_json j = Aws.Json.to_list PredictiveScalingMetricSpecification.of_json j
