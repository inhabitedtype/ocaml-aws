type t = UserAuthConfigInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map UserAuthConfigInfo.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list UserAuthConfigInfo.to_query v
let to_json v = `List (List.map UserAuthConfigInfo.to_json v)
let of_json j = Aws.Json.to_list UserAuthConfigInfo.of_json j
