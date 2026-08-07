open Aws.BaseTypes

type t =
  { resources_succeeded : Integer.t option
  ; resources_failed : Integer.t option
  ; resources_processing : Integer.t option
  ; resources_pending : Integer.t option
  }

let make
    ?resources_succeeded
    ?resources_failed
    ?resources_processing
    ?resources_pending
    () =
  { resources_succeeded; resources_failed; resources_processing; resources_pending }

let parse xml =
  Some
    { resources_succeeded =
        Aws.Util.option_bind (Aws.Xml.member "ResourcesSucceeded" xml) Integer.parse
    ; resources_failed =
        Aws.Util.option_bind (Aws.Xml.member "ResourcesFailed" xml) Integer.parse
    ; resources_processing =
        Aws.Util.option_bind (Aws.Xml.member "ResourcesProcessing" xml) Integer.parse
    ; resources_pending =
        Aws.Util.option_bind (Aws.Xml.member "ResourcesPending" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resources_pending (fun f ->
             Aws.Query.Pair ("ResourcesPending", Integer.to_query f))
       ; Aws.Util.option_map v.resources_processing (fun f ->
             Aws.Query.Pair ("ResourcesProcessing", Integer.to_query f))
       ; Aws.Util.option_map v.resources_failed (fun f ->
             Aws.Query.Pair ("ResourcesFailed", Integer.to_query f))
       ; Aws.Util.option_map v.resources_succeeded (fun f ->
             Aws.Query.Pair ("ResourcesSucceeded", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resources_pending (fun f ->
             "ResourcesPending", Integer.to_json f)
       ; Aws.Util.option_map v.resources_processing (fun f ->
             "ResourcesProcessing", Integer.to_json f)
       ; Aws.Util.option_map v.resources_failed (fun f ->
             "ResourcesFailed", Integer.to_json f)
       ; Aws.Util.option_map v.resources_succeeded (fun f ->
             "ResourcesSucceeded", Integer.to_json f)
       ])

let of_json j =
  { resources_succeeded =
      Aws.Util.option_map (Aws.Json.lookup j "ResourcesSucceeded") Integer.of_json
  ; resources_failed =
      Aws.Util.option_map (Aws.Json.lookup j "ResourcesFailed") Integer.of_json
  ; resources_processing =
      Aws.Util.option_map (Aws.Json.lookup j "ResourcesProcessing") Integer.of_json
  ; resources_pending =
      Aws.Util.option_map (Aws.Json.lookup j "ResourcesPending") Integer.of_json
  }
