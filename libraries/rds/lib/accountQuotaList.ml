type t = AccountQuota.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AccountQuota.parse (Aws.Xml.members "AccountQuota" xml))

let to_query v = Aws.Query.to_query_list AccountQuota.to_query v
let to_json v = `List (List.map AccountQuota.to_json v)
let of_json j = Aws.Json.to_list AccountQuota.of_json j
