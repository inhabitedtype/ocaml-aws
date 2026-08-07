type t = PartialFailure.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PartialFailure.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list PartialFailure.to_query v
let to_json v = `List (List.map PartialFailure.to_json v)
let of_json j = Aws.Json.to_list PartialFailure.of_json j
