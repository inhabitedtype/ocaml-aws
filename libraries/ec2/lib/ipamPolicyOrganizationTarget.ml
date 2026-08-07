open Aws.BaseTypes

type t = { organization_target_id : String.t option }

let make ?organization_target_id () = { organization_target_id }

let parse xml =
  Some
    { organization_target_id =
        Aws.Util.option_bind (Aws.Xml.member "organizationTargetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organization_target_id (fun f ->
             Aws.Query.Pair ("OrganizationTargetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organization_target_id (fun f ->
             "organizationTargetId", String.to_json f)
       ])

let of_json j =
  { organization_target_id =
      Aws.Util.option_map (Aws.Json.lookup j "organizationTargetId") String.of_json
  }
