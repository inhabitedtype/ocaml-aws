type t = DashboardEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DashboardEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DashboardEntry.to_query v
let to_json v = `List (List.map DashboardEntry.to_json v)
let of_json j = Aws.Json.to_list DashboardEntry.of_json j
