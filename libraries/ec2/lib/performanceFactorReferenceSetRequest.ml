type t = PerformanceFactorReferenceRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map PerformanceFactorReferenceRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PerformanceFactorReferenceRequest.to_query v
let to_json v = `List (List.map PerformanceFactorReferenceRequest.to_json v)
let of_json j = Aws.Json.to_list PerformanceFactorReferenceRequest.of_json j
