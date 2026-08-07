type t = DashboardDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DashboardDetail.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DashboardDetail.to_query v
let to_json v = `List (List.map DashboardDetail.to_json v)
let of_json j = Aws.Json.to_list DashboardDetail.of_json j
