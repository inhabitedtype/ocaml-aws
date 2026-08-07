open Aws.BaseTypes

type t =
  { stack_name : String.t
  ; logical_resource_id : String.t
  ; unique_id : String.t
  ; status : ResourceSignalStatus.t
  }

let make ~stack_name ~logical_resource_id ~unique_id ~status () =
  { stack_name; logical_resource_id; unique_id; status }

let parse xml =
  Some
    { stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; logical_resource_id =
        Aws.Xml.required
          "LogicalResourceId"
          (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
    ; unique_id =
        Aws.Xml.required
          "UniqueId"
          (Aws.Util.option_bind (Aws.Xml.member "UniqueId" xml) String.parse)
    ; status =
        Aws.Xml.required
          "Status"
          (Aws.Util.option_bind (Aws.Xml.member "Status" xml) ResourceSignalStatus.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Status", ResourceSignalStatus.to_query v.status))
       ; Some (Aws.Query.Pair ("UniqueId", String.to_query v.unique_id))
       ; Some
           (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Status", ResourceSignalStatus.to_json v.status)
       ; Some ("UniqueId", String.to_json v.unique_id)
       ; Some ("LogicalResourceId", String.to_json v.logical_resource_id)
       ; Some ("StackName", String.to_json v.stack_name)
       ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; logical_resource_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
  ; unique_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UniqueId"))
  ; status =
      ResourceSignalStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Status"))
  }
