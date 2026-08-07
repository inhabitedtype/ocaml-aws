open Aws.BaseTypes

type t =
  { action_id : String.t option
  ; title : String.t option
  ; description : String.t option
  ; operation : String.t option
  ; parameters : RecommendedActionParameterList.t
  ; apply_modes : StringList.t
  ; status : String.t option
  ; issue_details : IssueDetails.t option
  ; context_attributes : ContextAttributeList.t
  }

let make
    ?action_id
    ?title
    ?description
    ?operation
    ?(parameters = [])
    ?(apply_modes = [])
    ?status
    ?issue_details
    ?(context_attributes = [])
    () =
  { action_id
  ; title
  ; description
  ; operation
  ; parameters
  ; apply_modes
  ; status
  ; issue_details
  ; context_attributes
  }

let parse xml =
  Some
    { action_id = Aws.Util.option_bind (Aws.Xml.member "ActionId" xml) String.parse
    ; title = Aws.Util.option_bind (Aws.Xml.member "Title" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; operation = Aws.Util.option_bind (Aws.Xml.member "Operation" xml) String.parse
    ; parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Parameters" xml)
             RecommendedActionParameterList.parse)
    ; apply_modes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ApplyModes" xml) StringList.parse)
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; issue_details =
        Aws.Util.option_bind (Aws.Xml.member "IssueDetails" xml) IssueDetails.parse
    ; context_attributes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ContextAttributes" xml)
             ContextAttributeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ContextAttributes.member"
              , ContextAttributeList.to_query v.context_attributes ))
       ; Aws.Util.option_map v.issue_details (fun f ->
             Aws.Query.Pair ("IssueDetails", IssueDetails.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Some (Aws.Query.Pair ("ApplyModes.member", StringList.to_query v.apply_modes))
       ; Some
           (Aws.Query.Pair
              ("Parameters.member", RecommendedActionParameterList.to_query v.parameters))
       ; Aws.Util.option_map v.operation (fun f ->
             Aws.Query.Pair ("Operation", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.title (fun f ->
             Aws.Query.Pair ("Title", String.to_query f))
       ; Aws.Util.option_map v.action_id (fun f ->
             Aws.Query.Pair ("ActionId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ContextAttributes", ContextAttributeList.to_json v.context_attributes)
       ; Aws.Util.option_map v.issue_details (fun f ->
             "IssueDetails", IssueDetails.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Some ("ApplyModes", StringList.to_json v.apply_modes)
       ; Some ("Parameters", RecommendedActionParameterList.to_json v.parameters)
       ; Aws.Util.option_map v.operation (fun f -> "Operation", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.title (fun f -> "Title", String.to_json f)
       ; Aws.Util.option_map v.action_id (fun f -> "ActionId", String.to_json f)
       ])

let of_json j =
  { action_id = Aws.Util.option_map (Aws.Json.lookup j "ActionId") String.of_json
  ; title = Aws.Util.option_map (Aws.Json.lookup j "Title") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; operation = Aws.Util.option_map (Aws.Json.lookup j "Operation") String.of_json
  ; parameters =
      RecommendedActionParameterList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; apply_modes =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyModes"))
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; issue_details =
      Aws.Util.option_map (Aws.Json.lookup j "IssueDetails") IssueDetails.of_json
  ; context_attributes =
      ContextAttributeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ContextAttributes"))
  }
