open Aws.BaseTypes

type t = { cpu_credits : String.t }

let make ~cpu_credits () = { cpu_credits }

let parse xml =
  Some
    { cpu_credits =
        Aws.Xml.required
          "CpuCredits"
          (Aws.Util.option_bind (Aws.Xml.member "CpuCredits" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("CpuCredits", String.to_query v.cpu_credits)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("CpuCredits", String.to_json v.cpu_credits) ])

let of_json j =
  { cpu_credits = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CpuCredits"))
  }
