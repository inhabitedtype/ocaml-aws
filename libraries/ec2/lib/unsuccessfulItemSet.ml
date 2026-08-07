type t = UnsuccessfulItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map UnsuccessfulItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list UnsuccessfulItem.to_query v
let to_json v = `List (List.map UnsuccessfulItem.to_json v)
let of_json j = Aws.Json.to_list UnsuccessfulItem.of_json j
