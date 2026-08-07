open Aws.BaseTypes

type t =
  { enabled : Boolean.t option
  ; retain_stacks_on_account_removal : Boolean.t option
  ; depends_on : StackSetARNList.t
  }

let make ?enabled ?retain_stacks_on_account_removal ?(depends_on = []) () =
  { enabled; retain_stacks_on_account_removal; depends_on }

let parse xml =
  Some
    { enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
    ; retain_stacks_on_account_removal =
        Aws.Util.option_bind
          (Aws.Xml.member "RetainStacksOnAccountRemoval" xml)
          Boolean.parse
    ; depends_on =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "DependsOn" xml) StackSetARNList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DependsOn.member", StackSetARNList.to_query v.depends_on))
       ; Aws.Util.option_map v.retain_stacks_on_account_removal (fun f ->
             Aws.Query.Pair ("RetainStacksOnAccountRemoval", Boolean.to_query f))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DependsOn", StackSetARNList.to_json v.depends_on)
       ; Aws.Util.option_map v.retain_stacks_on_account_removal (fun f ->
             "RetainStacksOnAccountRemoval", Boolean.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
       ])

let of_json j =
  { enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
  ; retain_stacks_on_account_removal =
      Aws.Util.option_map
        (Aws.Json.lookup j "RetainStacksOnAccountRemoval")
        Boolean.of_json
  ; depends_on =
      StackSetARNList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DependsOn"))
  }
