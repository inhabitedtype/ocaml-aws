type t = DeclarativePoliciesReport.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DeclarativePoliciesReport.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DeclarativePoliciesReport.to_query v
let to_json v = `List (List.map DeclarativePoliciesReport.to_json v)
let of_json j = Aws.Json.to_list DeclarativePoliciesReport.of_json j
