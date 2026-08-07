open Aws.BaseTypes

type t =
  { type_ : ChangeType.t option
  ; hook_invocation_count : Integer.t option
  ; resource_change : ResourceChange.t option
  }

let make ?type_ ?hook_invocation_count ?resource_change () =
  { type_; hook_invocation_count; resource_change }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) ChangeType.parse
    ; hook_invocation_count =
        Aws.Util.option_bind (Aws.Xml.member "HookInvocationCount" xml) Integer.parse
    ; resource_change =
        Aws.Util.option_bind (Aws.Xml.member "ResourceChange" xml) ResourceChange.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_change (fun f ->
             Aws.Query.Pair ("ResourceChange", ResourceChange.to_query f))
       ; Aws.Util.option_map v.hook_invocation_count (fun f ->
             Aws.Query.Pair ("HookInvocationCount", Integer.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", ChangeType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_change (fun f ->
             "ResourceChange", ResourceChange.to_json f)
       ; Aws.Util.option_map v.hook_invocation_count (fun f ->
             "HookInvocationCount", Integer.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", ChangeType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") ChangeType.of_json
  ; hook_invocation_count =
      Aws.Util.option_map (Aws.Json.lookup j "HookInvocationCount") Integer.of_json
  ; resource_change =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceChange") ResourceChange.of_json
  }
