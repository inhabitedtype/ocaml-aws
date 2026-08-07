type t = Ipv4PrefixSpecificationResponse.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map Ipv4PrefixSpecificationResponse.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list Ipv4PrefixSpecificationResponse.to_query v
let to_json v = `List (List.map Ipv4PrefixSpecificationResponse.to_json v)
let of_json j = Aws.Json.to_list Ipv4PrefixSpecificationResponse.of_json j
