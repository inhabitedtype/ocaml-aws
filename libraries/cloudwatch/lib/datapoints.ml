type t = Datapoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Datapoint.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Datapoint.to_query v
let to_json v = `List (List.map Datapoint.to_json v)
let of_json j = Aws.Json.to_list Datapoint.of_json j
