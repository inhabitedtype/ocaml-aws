type t = HistoryRecord.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map HistoryRecord.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list HistoryRecord.to_query v
let to_json v = `List (List.map HistoryRecord.to_json v)
let of_json j = Aws.Json.to_list HistoryRecord.of_json j
