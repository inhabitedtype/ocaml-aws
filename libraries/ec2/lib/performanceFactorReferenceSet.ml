type t = PerformanceFactorReference.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map PerformanceFactorReference.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PerformanceFactorReference.to_query v
let to_json v = `List (List.map PerformanceFactorReference.to_json v)
let of_json j = Aws.Json.to_list PerformanceFactorReference.of_json j
