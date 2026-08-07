open Aws.BaseTypes

type t =
  { policy_a_r_n : String.t option
  ; alarms : Alarms.t
  }

let make ?policy_a_r_n ?(alarms = []) () = { policy_a_r_n; alarms }

let parse xml =
  Some
    { policy_a_r_n = Aws.Util.option_bind (Aws.Xml.member "PolicyARN" xml) String.parse
    ; alarms =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Alarms" xml) Alarms.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Alarms.member", Alarms.to_query v.alarms))
       ; Aws.Util.option_map v.policy_a_r_n (fun f ->
             Aws.Query.Pair ("PolicyARN", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Alarms", Alarms.to_json v.alarms)
       ; Aws.Util.option_map v.policy_a_r_n (fun f -> "PolicyARN", String.to_json f)
       ])

let of_json j =
  { policy_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "PolicyARN") String.of_json
  ; alarms = Alarms.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Alarms"))
  }
