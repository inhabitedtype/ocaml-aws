open Aws.BaseTypes

type t =
  { principal_type : PrincipalType.t option
  ; principal : String.t option
  ; service_permission_id : String.t option
  ; tags : TagList.t
  ; service_id : String.t option
  }

let make ?principal_type ?principal ?service_permission_id ?(tags = []) ?service_id () =
  { principal_type; principal; service_permission_id; tags; service_id }

let parse xml =
  Some
    { principal_type =
        Aws.Util.option_bind (Aws.Xml.member "principalType" xml) PrincipalType.parse
    ; principal = Aws.Util.option_bind (Aws.Xml.member "principal" xml) String.parse
    ; service_permission_id =
        Aws.Util.option_bind (Aws.Xml.member "servicePermissionId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; service_id = Aws.Util.option_bind (Aws.Xml.member "serviceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_id (fun f ->
             Aws.Query.Pair ("ServiceId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.service_permission_id (fun f ->
             Aws.Query.Pair ("ServicePermissionId", String.to_query f))
       ; Aws.Util.option_map v.principal (fun f ->
             Aws.Query.Pair ("Principal", String.to_query f))
       ; Aws.Util.option_map v.principal_type (fun f ->
             Aws.Query.Pair ("PrincipalType", PrincipalType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_id (fun f -> "serviceId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.service_permission_id (fun f ->
             "servicePermissionId", String.to_json f)
       ; Aws.Util.option_map v.principal (fun f -> "principal", String.to_json f)
       ; Aws.Util.option_map v.principal_type (fun f ->
             "principalType", PrincipalType.to_json f)
       ])

let of_json j =
  { principal_type =
      Aws.Util.option_map (Aws.Json.lookup j "principalType") PrincipalType.of_json
  ; principal = Aws.Util.option_map (Aws.Json.lookup j "principal") String.of_json
  ; service_permission_id =
      Aws.Util.option_map (Aws.Json.lookup j "servicePermissionId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; service_id = Aws.Util.option_map (Aws.Json.lookup j "serviceId") String.of_json
  }
