open Aws.BaseTypes

type t =
  { name : String.t
  ; policy : String.t option
  ; policy_arns : PolicyDescriptorListType.t
  ; duration_seconds : Integer.t option
  ; tags : TagListType.t
  }

let make ~name ?policy ?(policy_arns = []) ?duration_seconds ?(tags = []) () =
  { name; policy; policy_arns; duration_seconds; tags }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; policy = Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse
    ; policy_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PolicyArns" xml)
             PolicyDescriptorListType.parse)
    ; duration_seconds =
        Aws.Util.option_bind (Aws.Xml.member "DurationSeconds" xml) Integer.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagListType.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagListType.to_query v.tags))
       ; Aws.Util.option_map v.duration_seconds (fun f ->
             Aws.Query.Pair ("DurationSeconds", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("PolicyArns.member", PolicyDescriptorListType.to_query v.policy_arns))
       ; Aws.Util.option_map v.policy (fun f ->
             Aws.Query.Pair ("Policy", String.to_query f))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagListType.to_json v.tags)
       ; Aws.Util.option_map v.duration_seconds (fun f ->
             "DurationSeconds", Integer.to_json f)
       ; Some ("PolicyArns", PolicyDescriptorListType.to_json v.policy_arns)
       ; Aws.Util.option_map v.policy (fun f -> "Policy", String.to_json f)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; policy = Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json
  ; policy_arns =
      PolicyDescriptorListType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyArns"))
  ; duration_seconds =
      Aws.Util.option_map (Aws.Json.lookup j "DurationSeconds") Integer.of_json
  ; tags = TagListType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
