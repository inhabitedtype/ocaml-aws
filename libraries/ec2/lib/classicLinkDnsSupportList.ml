type t = ClassicLinkDnsSupport.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ClassicLinkDnsSupport.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ClassicLinkDnsSupport.to_query v
let to_json v = `List (List.map ClassicLinkDnsSupport.to_json v)
let of_json j = Aws.Json.to_list ClassicLinkDnsSupport.of_json j
