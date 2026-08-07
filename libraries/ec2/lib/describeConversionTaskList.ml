type t = ConversionTask.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ConversionTask.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ConversionTask.to_query v
let to_json v = `List (List.map ConversionTask.to_json v)
let of_json j = Aws.Json.to_list ConversionTask.of_json j
