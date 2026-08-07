type t = Parameter.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Parameter.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Parameter.to_query v
let to_json v = `List (List.map Parameter.to_json v)
let of_json j = Aws.Json.to_list Parameter.of_json j
