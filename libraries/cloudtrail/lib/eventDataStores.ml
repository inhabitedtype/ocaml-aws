type t = EventDataStore.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map EventDataStore.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list EventDataStore.to_query v
let to_json v = `List (List.map EventDataStore.to_json v)
let of_json j = Aws.Json.to_list EventDataStore.of_json j
