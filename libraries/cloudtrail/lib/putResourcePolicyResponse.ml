open Aws.BaseTypes

type t =
  { resource_arn : String.t option
  ; resource_policy : String.t option
  ; delegated_admin_resource_policy : String.t option
  }

let make ?resource_arn ?resource_policy ?delegated_admin_resource_policy () =
  { resource_arn; resource_policy; delegated_admin_resource_policy }

let parse xml =
  Some
    { resource_arn = Aws.Util.option_bind (Aws.Xml.member "ResourceArn" xml) String.parse
    ; resource_policy =
        Aws.Util.option_bind (Aws.Xml.member "ResourcePolicy" xml) String.parse
    ; delegated_admin_resource_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "DelegatedAdminResourcePolicy" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delegated_admin_resource_policy (fun f ->
             Aws.Query.Pair ("DelegatedAdminResourcePolicy", String.to_query f))
       ; Aws.Util.option_map v.resource_policy (fun f ->
             Aws.Query.Pair ("ResourcePolicy", String.to_query f))
       ; Aws.Util.option_map v.resource_arn (fun f ->
             Aws.Query.Pair ("ResourceArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delegated_admin_resource_policy (fun f ->
             "DelegatedAdminResourcePolicy", String.to_json f)
       ; Aws.Util.option_map v.resource_policy (fun f ->
             "ResourcePolicy", String.to_json f)
       ; Aws.Util.option_map v.resource_arn (fun f -> "ResourceArn", String.to_json f)
       ])

let of_json j =
  { resource_arn = Aws.Util.option_map (Aws.Json.lookup j "ResourceArn") String.of_json
  ; resource_policy =
      Aws.Util.option_map (Aws.Json.lookup j "ResourcePolicy") String.of_json
  ; delegated_admin_resource_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "DelegatedAdminResourcePolicy")
        String.of_json
  }
