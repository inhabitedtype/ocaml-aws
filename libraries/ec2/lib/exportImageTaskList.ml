type t = ExportImageTask.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ExportImageTask.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ExportImageTask.to_query v
let to_json v = `List (List.map ExportImageTask.to_json v)
let of_json j = Aws.Json.to_list ExportImageTask.of_json j
