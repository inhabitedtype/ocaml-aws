type t = ProvidedContext.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ProvidedContext.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ProvidedContext.to_query v
let to_json v = `List (List.map ProvidedContext.to_json v)
let of_json j = Aws.Json.to_list ProvidedContext.of_json j
