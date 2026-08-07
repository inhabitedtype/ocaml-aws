type t = RuleOption.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RuleOption.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RuleOption.to_query v
let to_json v = `List (List.map RuleOption.to_json v)
let of_json j = Aws.Json.to_list RuleOption.of_json j
