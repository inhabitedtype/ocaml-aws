type t = RollbackTrigger.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RollbackTrigger.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list RollbackTrigger.to_query v
let to_json v = `List (List.map RollbackTrigger.to_json v)
let of_json j = Aws.Json.to_list RollbackTrigger.of_json j
