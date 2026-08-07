type t = RouteServerRouteInstallationDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map RouteServerRouteInstallationDetail.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RouteServerRouteInstallationDetail.to_query v
let to_json v = `List (List.map RouteServerRouteInstallationDetail.to_json v)
let of_json j = Aws.Json.to_list RouteServerRouteInstallationDetail.of_json j
