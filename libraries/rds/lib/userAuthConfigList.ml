type t = UserAuthConfig.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map UserAuthConfig.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list UserAuthConfig.to_query v
let to_json v = `List (List.map UserAuthConfig.to_json v)
let of_json j = Aws.Json.to_list UserAuthConfig.of_json j
