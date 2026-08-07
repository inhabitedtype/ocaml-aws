type t = ExportTask.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ExportTask.parse (Aws.Xml.members "ExportTask" xml))

let to_query v = Aws.Query.to_query_list ExportTask.to_query v
let to_json v = `List (List.map ExportTask.to_json v)
let of_json j = Aws.Json.to_list ExportTask.of_json j
