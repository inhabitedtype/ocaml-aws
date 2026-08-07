type t = DescribeDBLogFilesDetails.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       DescribeDBLogFilesDetails.parse
       (Aws.Xml.members "DescribeDBLogFilesDetails" xml))

let to_query v = Aws.Query.to_query_list DescribeDBLogFilesDetails.to_query v
let to_json v = `List (List.map DescribeDBLogFilesDetails.to_json v)
let of_json j = Aws.Json.to_list DescribeDBLogFilesDetails.of_json j
