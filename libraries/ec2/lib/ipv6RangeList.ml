type t = Ipv6Range.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Ipv6Range.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list Ipv6Range.to_query v
let to_json v = `List (List.map Ipv6Range.to_json v)
let of_json j = Aws.Json.to_list Ipv6Range.of_json j
