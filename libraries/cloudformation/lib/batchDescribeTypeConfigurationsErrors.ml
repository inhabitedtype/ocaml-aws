type t = BatchDescribeTypeConfigurationsError.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map BatchDescribeTypeConfigurationsError.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list BatchDescribeTypeConfigurationsError.to_query v
let to_json v = `List (List.map BatchDescribeTypeConfigurationsError.to_json v)
let of_json j = Aws.Json.to_list BatchDescribeTypeConfigurationsError.of_json j
