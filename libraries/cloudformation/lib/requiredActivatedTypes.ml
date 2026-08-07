type t = RequiredActivatedType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map RequiredActivatedType.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list RequiredActivatedType.to_query v
let to_json v = `List (List.map RequiredActivatedType.to_json v)
let of_json j = Aws.Json.to_list RequiredActivatedType.of_json j
