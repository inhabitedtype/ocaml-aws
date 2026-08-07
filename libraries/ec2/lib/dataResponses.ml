type t = DataResponse.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DataResponse.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DataResponse.to_query v
let to_json v = `List (List.map DataResponse.to_json v)
let of_json j = Aws.Json.to_list DataResponse.of_json j
