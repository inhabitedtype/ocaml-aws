type t = CacheSecurityGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CacheSecurityGroup.parse (Aws.Xml.members "CacheSecurityGroup" xml))

let to_query v = Aws.Query.to_query_list CacheSecurityGroup.to_query v
let to_json v = `List (List.map CacheSecurityGroup.to_json v)
let of_json j = Aws.Json.to_list CacheSecurityGroup.of_json j
