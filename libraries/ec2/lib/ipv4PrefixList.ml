type t = Ipv4PrefixSpecificationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map Ipv4PrefixSpecificationRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list Ipv4PrefixSpecificationRequest.to_query v
let to_json v = `List (List.map Ipv4PrefixSpecificationRequest.to_json v)
let of_json j = Aws.Json.to_list Ipv4PrefixSpecificationRequest.of_json j
