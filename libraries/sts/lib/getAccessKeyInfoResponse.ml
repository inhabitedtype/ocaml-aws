open Aws.BaseTypes

type t = { account : String.t option }

let make ?account () = { account }

let parse xml =
  Some { account = Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.account (fun f ->
             Aws.Query.Pair ("Account", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.account (fun f -> "Account", String.to_json f) ])

let of_json j =
  { account = Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json }
