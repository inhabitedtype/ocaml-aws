open Aws.BaseTypes

type t =
  { stack_name : String.t
  ; logical_resource_ids : LogicalResourceIds.t
  }

let make ~stack_name ?(logical_resource_ids = []) () =
  { stack_name; logical_resource_ids }

let parse xml =
  Some
    { stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    ; logical_resource_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogicalResourceIds" xml)
             LogicalResourceIds.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "LogicalResourceIds.member"
              , LogicalResourceIds.to_query v.logical_resource_ids ))
       ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LogicalResourceIds", LogicalResourceIds.to_json v.logical_resource_ids)
       ; Some ("StackName", String.to_json v.stack_name)
       ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  ; logical_resource_ids =
      LogicalResourceIds.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceIds"))
  }
