open Aws.BaseTypes

type t =
  { source_member : String.t option
  ; target_member : String.t option
  ; status : String.t option
  }

let make ?source_member ?target_member ?status () =
  { source_member; target_member; status }

let parse xml =
  Some
    { source_member =
        Aws.Util.option_bind (Aws.Xml.member "SourceMember" xml) String.parse
    ; target_member =
        Aws.Util.option_bind (Aws.Xml.member "TargetMember" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.target_member (fun f ->
             Aws.Query.Pair ("TargetMember", String.to_query f))
       ; Aws.Util.option_map v.source_member (fun f ->
             Aws.Query.Pair ("SourceMember", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.target_member (fun f -> "TargetMember", String.to_json f)
       ; Aws.Util.option_map v.source_member (fun f -> "SourceMember", String.to_json f)
       ])

let of_json j =
  { source_member = Aws.Util.option_map (Aws.Json.lookup j "SourceMember") String.of_json
  ; target_member = Aws.Util.option_map (Aws.Json.lookup j "TargetMember") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  }
