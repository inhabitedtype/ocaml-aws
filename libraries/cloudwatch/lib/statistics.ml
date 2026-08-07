type t = Statistic.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Statistic.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Statistic.to_query v
let to_json v = `List (List.map Statistic.to_json v)
let of_json j = Aws.Json.to_list Statistic.of_json j
