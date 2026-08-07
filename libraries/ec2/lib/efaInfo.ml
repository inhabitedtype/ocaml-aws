open Aws.BaseTypes

type t = { maximum_efa_interfaces : Integer.t option }

let make ?maximum_efa_interfaces () = { maximum_efa_interfaces }

let parse xml =
  Some
    { maximum_efa_interfaces =
        Aws.Util.option_bind (Aws.Xml.member "maximumEfaInterfaces" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_efa_interfaces (fun f ->
             Aws.Query.Pair ("MaximumEfaInterfaces", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.maximum_efa_interfaces (fun f ->
             "maximumEfaInterfaces", Integer.to_json f)
       ])

let of_json j =
  { maximum_efa_interfaces =
      Aws.Util.option_map (Aws.Json.lookup j "maximumEfaInterfaces") Integer.of_json
  }
