type t = LoadForecast.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LoadForecast.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LoadForecast.to_query v
let to_json v = `List (List.map LoadForecast.to_json v)
let of_json j = Aws.Json.to_list LoadForecast.of_json j
