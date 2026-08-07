type t = { internet_gateway : InternetGateway.t option }

let make ?internet_gateway () = { internet_gateway }

let parse xml =
  Some
    { internet_gateway =
        Aws.Util.option_bind (Aws.Xml.member "internetGateway" xml) InternetGateway.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.internet_gateway (fun f ->
             Aws.Query.Pair ("InternetGateway", InternetGateway.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.internet_gateway (fun f ->
             "internetGateway", InternetGateway.to_json f)
       ])

let of_json j =
  { internet_gateway =
      Aws.Util.option_map (Aws.Json.lookup j "internetGateway") InternetGateway.of_json
  }
