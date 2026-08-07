type t = DnsEntry.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map DnsEntry.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list DnsEntry.to_query v
let to_json v = `List (List.map DnsEntry.to_json v)
let of_json j = Aws.Json.to_list DnsEntry.of_json j
