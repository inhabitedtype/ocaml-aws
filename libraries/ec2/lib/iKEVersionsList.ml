type t = IKEVersionsListValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IKEVersionsListValue.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IKEVersionsListValue.to_query v
let to_json v = `List (List.map IKEVersionsListValue.to_json v)
let of_json j = Aws.Json.to_list IKEVersionsListValue.of_json j
