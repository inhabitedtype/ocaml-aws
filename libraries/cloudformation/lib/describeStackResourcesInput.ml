open Aws.BaseTypes

type t =
  { stack_name : String.t option
  ; logical_resource_id : String.t option
  ; physical_resource_id : String.t option
  }

let make ?stack_name ?logical_resource_id ?physical_resource_id () =
  { stack_name; logical_resource_id; physical_resource_id }

let parse xml =
  Some
    { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; logical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
    ; physical_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.physical_resource_id (fun f ->
             Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             Aws.Query.Pair ("LogicalResourceId", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.physical_resource_id (fun f ->
             "PhysicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.logical_resource_id (fun f ->
             "LogicalResourceId", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ])

let of_json j =
  { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; logical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
  ; physical_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
  }
