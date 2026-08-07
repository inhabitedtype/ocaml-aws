type t = AdditionalDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AdditionalDetail.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AdditionalDetail.to_query v
let to_json v = `List (List.map AdditionalDetail.to_json v)
let of_json j = Aws.Json.to_list AdditionalDetail.of_json j
