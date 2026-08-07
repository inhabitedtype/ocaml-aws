type t =
  { listener : Listener.t option
  ; policy_names : PolicyNames.t
  }

let make ?listener ?(policy_names = []) () = { listener; policy_names }

let parse xml =
  Some
    { listener = Aws.Util.option_bind (Aws.Xml.member "Listener" xml) Listener.parse
    ; policy_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
       ; Aws.Util.option_map v.listener (fun f ->
             Aws.Query.Pair ("Listener", Listener.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
       ; Aws.Util.option_map v.listener (fun f -> "Listener", Listener.to_json f)
       ])

let of_json j =
  { listener = Aws.Util.option_map (Aws.Json.lookup j "Listener") Listener.of_json
  ; policy_names =
      PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
  }
