open Aws.BaseTypes

type t = { cpu_credits : String.t option }

let make ?cpu_credits () = { cpu_credits }

let parse xml =
  Some
    { cpu_credits = Aws.Util.option_bind (Aws.Xml.member "cpuCredits" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cpu_credits (fun f ->
             Aws.Query.Pair ("CpuCredits", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cpu_credits (fun f -> "cpuCredits", String.to_json f) ])

let of_json j =
  { cpu_credits = Aws.Util.option_map (Aws.Json.lookup j "cpuCredits") String.of_json }
