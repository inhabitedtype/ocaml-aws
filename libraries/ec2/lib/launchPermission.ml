open Aws.BaseTypes

type t =
  { organization_arn : String.t option
  ; organizational_unit_arn : String.t option
  ; user_id : String.t option
  ; group : PermissionGroup.t option
  }

let make ?organization_arn ?organizational_unit_arn ?user_id ?group () =
  { organization_arn; organizational_unit_arn; user_id; group }

let parse xml =
  Some
    { organization_arn =
        Aws.Util.option_bind (Aws.Xml.member "organizationArn" xml) String.parse
    ; organizational_unit_arn =
        Aws.Util.option_bind (Aws.Xml.member "organizationalUnitArn" xml) String.parse
    ; user_id = Aws.Util.option_bind (Aws.Xml.member "userId" xml) String.parse
    ; group = Aws.Util.option_bind (Aws.Xml.member "group" xml) PermissionGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group (fun f ->
             Aws.Query.Pair ("Group", PermissionGroup.to_query f))
       ; Aws.Util.option_map v.user_id (fun f ->
             Aws.Query.Pair ("UserId", String.to_query f))
       ; Aws.Util.option_map v.organizational_unit_arn (fun f ->
             Aws.Query.Pair ("OrganizationalUnitArn", String.to_query f))
       ; Aws.Util.option_map v.organization_arn (fun f ->
             Aws.Query.Pair ("OrganizationArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group (fun f -> "group", PermissionGroup.to_json f)
       ; Aws.Util.option_map v.user_id (fun f -> "userId", String.to_json f)
       ; Aws.Util.option_map v.organizational_unit_arn (fun f ->
             "organizationalUnitArn", String.to_json f)
       ; Aws.Util.option_map v.organization_arn (fun f ->
             "organizationArn", String.to_json f)
       ])

let of_json j =
  { organization_arn =
      Aws.Util.option_map (Aws.Json.lookup j "organizationArn") String.of_json
  ; organizational_unit_arn =
      Aws.Util.option_map (Aws.Json.lookup j "organizationalUnitArn") String.of_json
  ; user_id = Aws.Util.option_map (Aws.Json.lookup j "userId") String.of_json
  ; group = Aws.Util.option_map (Aws.Json.lookup j "group") PermissionGroup.of_json
  }
