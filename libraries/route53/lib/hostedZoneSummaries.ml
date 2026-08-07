type t = HostedZoneSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map HostedZoneSummary.parse (Aws.Xml.members "HostedZoneSummary" xml))

let to_query v = Aws.Query.to_query_list HostedZoneSummary.to_query v
let to_json v = `List (List.map HostedZoneSummary.to_json v)
let of_json j = Aws.Json.to_list HostedZoneSummary.of_json j
