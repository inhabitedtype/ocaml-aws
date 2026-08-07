type t = IKEVersionsRequestListValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IKEVersionsRequestListValue.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IKEVersionsRequestListValue.to_query v
let to_json v = `List (List.map IKEVersionsRequestListValue.to_json v)
let of_json j = Aws.Json.to_list IKEVersionsRequestListValue.of_json j
