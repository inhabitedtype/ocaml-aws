open Aws.BaseTypes

type t =
  { target_type : HookTargetType.t
  ; target_type_name : String.t
  ; target_id : String.t
  ; action : HookTargetAction.t
  }

let make ~target_type ~target_type_name ~target_id ~action () =
  { target_type; target_type_name; target_id; action }

let parse xml =
  Some
    { target_type =
        Aws.Xml.required
          "TargetType"
          (Aws.Util.option_bind (Aws.Xml.member "TargetType" xml) HookTargetType.parse)
    ; target_type_name =
        Aws.Xml.required
          "TargetTypeName"
          (Aws.Util.option_bind (Aws.Xml.member "TargetTypeName" xml) String.parse)
    ; target_id =
        Aws.Xml.required
          "TargetId"
          (Aws.Util.option_bind (Aws.Xml.member "TargetId" xml) String.parse)
    ; action =
        Aws.Xml.required
          "Action"
          (Aws.Util.option_bind (Aws.Xml.member "Action" xml) HookTargetAction.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Action", HookTargetAction.to_query v.action))
       ; Some (Aws.Query.Pair ("TargetId", String.to_query v.target_id))
       ; Some (Aws.Query.Pair ("TargetTypeName", String.to_query v.target_type_name))
       ; Some (Aws.Query.Pair ("TargetType", HookTargetType.to_query v.target_type))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Action", HookTargetAction.to_json v.action)
       ; Some ("TargetId", String.to_json v.target_id)
       ; Some ("TargetTypeName", String.to_json v.target_type_name)
       ; Some ("TargetType", HookTargetType.to_json v.target_type)
       ])

let of_json j =
  { target_type =
      HookTargetType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetType"))
  ; target_type_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetTypeName"))
  ; target_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetId"))
  ; action =
      HookTargetAction.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Action"))
  }
