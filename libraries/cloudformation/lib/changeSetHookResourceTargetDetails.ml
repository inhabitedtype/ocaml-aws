open Aws.BaseTypes

type t =
  { logical_resource_id : String.t option
  ; resource_type : String.t option
  ; resource_action : ChangeAction.t option
  }

let make ?logical_resource_id ?resource_type ?resource_action () =
  { logical_resource_id; resource_type; resource_action }

let parse xml =
  Some
    { logical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
    ; resource_action =
        Aws.Util.option_bind (Aws.Xml.member "ResourceAction" xml) ChangeAction.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_action (fun f ->
             Aws.Query.Pair ("ResourceAction", ChangeAction.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             Aws.Query.Pair ("LogicalResourceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_action (fun f ->
             "ResourceAction", ChangeAction.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             "LogicalResourceId", String.to_json f)
       ])

let of_json j =
  { logical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
  ; resource_type = Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
  ; resource_action =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceAction") ChangeAction.of_json
  }
