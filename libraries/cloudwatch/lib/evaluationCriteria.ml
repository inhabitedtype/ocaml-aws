type t = { prom_q_l_criteria : AlarmPromQLCriteria.t option }

let make ?prom_q_l_criteria () = { prom_q_l_criteria }

let parse xml =
  Some
    { prom_q_l_criteria =
        Aws.Util.option_bind
          (Aws.Xml.member "PromQLCriteria" xml)
          AlarmPromQLCriteria.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.prom_q_l_criteria (fun f ->
             Aws.Query.Pair ("PromQLCriteria", AlarmPromQLCriteria.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.prom_q_l_criteria (fun f ->
             "PromQLCriteria", AlarmPromQLCriteria.to_json f)
       ])

let of_json j =
  { prom_q_l_criteria =
      Aws.Util.option_map (Aws.Json.lookup j "PromQLCriteria") AlarmPromQLCriteria.of_json
  }
