type t = EC2SecurityGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map EC2SecurityGroup.parse (Aws.Xml.members "EC2SecurityGroup" xml))

let to_query v = Aws.Query.to_query_list EC2SecurityGroup.to_query v
let to_json v = `List (List.map EC2SecurityGroup.to_json v)
let of_json j = Aws.Json.to_list EC2SecurityGroup.of_json j
