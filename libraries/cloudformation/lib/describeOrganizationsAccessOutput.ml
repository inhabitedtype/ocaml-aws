type t = { status : OrganizationStatus.t option }

let make ?status () = { status }

let parse xml =
  Some
    { status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) OrganizationStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", OrganizationStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", OrganizationStatus.to_json f) ])

let of_json j =
  { status = Aws.Util.option_map (Aws.Json.lookup j "Status") OrganizationStatus.of_json }
