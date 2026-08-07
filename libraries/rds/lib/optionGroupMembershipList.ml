type t = OptionGroupMembership.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map OptionGroupMembership.parse (Aws.Xml.members "OptionGroupMembership" xml))

let to_query v = Aws.Query.to_query_list OptionGroupMembership.to_query v
let to_json v = `List (List.map OptionGroupMembership.to_json v)
let of_json j = Aws.Json.to_list OptionGroupMembership.of_json j
