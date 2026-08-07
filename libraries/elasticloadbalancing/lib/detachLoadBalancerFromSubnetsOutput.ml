type t = { subnets : Subnets.t }

let make ?(subnets = []) () = { subnets }

let parse xml =
  Some
    { subnets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) Subnets.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Subnets.member", Subnets.to_query v.subnets)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Subnets", Subnets.to_json v.subnets) ])

let of_json j =
  { subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets")) }
