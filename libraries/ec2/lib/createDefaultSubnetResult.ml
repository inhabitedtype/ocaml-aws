type t = { subnet : Subnet.t option }

let make ?subnet () = { subnet }

let parse xml =
  Some { subnet = Aws.Util.option_bind (Aws.Xml.member "subnet" xml) Subnet.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet (fun f ->
             Aws.Query.Pair ("Subnet", Subnet.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet (fun f -> "subnet", Subnet.to_json f) ])

let of_json j =
  { subnet = Aws.Util.option_map (Aws.Json.lookup j "subnet") Subnet.of_json }
