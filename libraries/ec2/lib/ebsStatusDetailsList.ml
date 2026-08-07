type t = EbsStatusDetails.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map EbsStatusDetails.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list EbsStatusDetails.to_query v
let to_json v = `List (List.map EbsStatusDetails.to_json v)
let of_json j = Aws.Json.to_list EbsStatusDetails.of_json j
