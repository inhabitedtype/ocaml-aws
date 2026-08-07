type t = ConfigureShard.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ConfigureShard.parse (Aws.Xml.members "ConfigureShard" xml))

let to_query v = Aws.Query.to_query_list ConfigureShard.to_query v
let to_json v = `List (List.map ConfigureShard.to_json v)
let of_json j = Aws.Json.to_list ConfigureShard.of_json j
