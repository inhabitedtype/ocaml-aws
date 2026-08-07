open Aws.BaseTypes

type t =
  { name : String.t
  ; description : String.t option
  ; rule : Rule.t
  ; mute_targets : MuteTargets.t option
  ; tags : TagList.t
  ; start_date : DateTime.t option
  ; expire_date : DateTime.t option
  }

let make ~name ?description ~rule ?mute_targets ?(tags = []) ?start_date ?expire_date () =
  { name; description; rule; mute_targets; tags; start_date; expire_date }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; rule =
        Aws.Xml.required
          "Rule"
          (Aws.Util.option_bind (Aws.Xml.member "Rule" xml) Rule.parse)
    ; mute_targets =
        Aws.Util.option_bind (Aws.Xml.member "MuteTargets" xml) MuteTargets.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "StartDate" xml) DateTime.parse
    ; expire_date = Aws.Util.option_bind (Aws.Xml.member "ExpireDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.expire_date (fun f ->
             Aws.Query.Pair ("ExpireDate", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.mute_targets (fun f ->
             Aws.Query.Pair ("MuteTargets", MuteTargets.to_query f))
       ; Some (Aws.Query.Pair ("Rule", Rule.to_query v.rule))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.expire_date (fun f -> "ExpireDate", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "StartDate", DateTime.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.mute_targets (fun f ->
             "MuteTargets", MuteTargets.to_json f)
       ; Some ("Rule", Rule.to_json v.rule)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; rule = Rule.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Rule"))
  ; mute_targets =
      Aws.Util.option_map (Aws.Json.lookup j "MuteTargets") MuteTargets.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "StartDate") DateTime.of_json
  ; expire_date = Aws.Util.option_map (Aws.Json.lookup j "ExpireDate") DateTime.of_json
  }
