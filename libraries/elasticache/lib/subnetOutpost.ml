open Aws.BaseTypes

type t = { subnet_outpost_arn : String.t option }

let make ?subnet_outpost_arn () = { subnet_outpost_arn }

let parse xml =
  Some
    { subnet_outpost_arn =
        Aws.Util.option_bind (Aws.Xml.member "SubnetOutpostArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_outpost_arn (fun f ->
             Aws.Query.Pair ("SubnetOutpostArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_outpost_arn (fun f ->
             "SubnetOutpostArn", String.to_json f)
       ])

let of_json j =
  { subnet_outpost_arn =
      Aws.Util.option_map (Aws.Json.lookup j "SubnetOutpostArn") String.of_json
  }
