type t = ManagedRuleDescription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ManagedRuleDescription.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ManagedRuleDescription.to_query v
let to_json v = `List (List.map ManagedRuleDescription.to_json v)
let of_json j = Aws.Json.to_list ManagedRuleDescription.of_json j
