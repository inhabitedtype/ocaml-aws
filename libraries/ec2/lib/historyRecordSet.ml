type t = HistoryRecordEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map HistoryRecordEntry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list HistoryRecordEntry.to_query v
let to_json v = `List (List.map HistoryRecordEntry.to_json v)
let of_json j = Aws.Json.to_list HistoryRecordEntry.of_json j
