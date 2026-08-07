type t = DataResource.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DataResource.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DataResource.to_query v
let to_json v = `List (List.map DataResource.to_json v)
let of_json j = Aws.Json.to_list DataResource.of_json j
