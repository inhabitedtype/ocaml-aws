type t = { terminate_hook_abandon : RetentionAction.t option }

let make ?terminate_hook_abandon () = { terminate_hook_abandon }

let parse xml =
  Some
    { terminate_hook_abandon =
        Aws.Util.option_bind
          (Aws.Xml.member "TerminateHookAbandon" xml)
          RetentionAction.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.terminate_hook_abandon (fun f ->
             Aws.Query.Pair ("TerminateHookAbandon", RetentionAction.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.terminate_hook_abandon (fun f ->
             "TerminateHookAbandon", RetentionAction.to_json f)
       ])

let of_json j =
  { terminate_hook_abandon =
      Aws.Util.option_map
        (Aws.Json.lookup j "TerminateHookAbandon")
        RetentionAction.of_json
  }
