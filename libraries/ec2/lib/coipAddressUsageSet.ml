type t = CoipAddressUsage.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CoipAddressUsage.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CoipAddressUsage.to_query v
let to_json v = `List (List.map CoipAddressUsage.to_json v)
let of_json j = Aws.Json.to_list CoipAddressUsage.of_json j
