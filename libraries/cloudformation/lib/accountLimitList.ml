type t = AccountLimit.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AccountLimit.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AccountLimit.to_query v
let to_json v = `List (List.map AccountLimit.to_json v)
let of_json j = Aws.Json.to_list AccountLimit.of_json j
