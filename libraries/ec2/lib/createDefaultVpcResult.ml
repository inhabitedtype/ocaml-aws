type t = { vpc : Vpc.t option }

let make ?vpc () = { vpc }
let parse xml = Some { vpc = Aws.Util.option_bind (Aws.Xml.member "vpc" xml) Vpc.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc (fun f -> Aws.Query.Pair ("Vpc", Vpc.to_query f)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc (fun f -> "vpc", Vpc.to_json f) ])

let of_json j = { vpc = Aws.Util.option_map (Aws.Json.lookup j "vpc") Vpc.of_json }
