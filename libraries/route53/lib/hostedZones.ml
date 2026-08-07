type t = HostedZone.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map HostedZone.parse (Aws.Xml.members "HostedZone" xml))

let to_query v = Aws.Query.to_query_list HostedZone.to_query v
let to_json v = `List (List.map HostedZone.to_json v)
let of_json j = Aws.Json.to_list HostedZone.of_json j
