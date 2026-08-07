open Aws.BaseTypes

type t = { organizations_entity_path : String.t option }

let make ?organizations_entity_path () = { organizations_entity_path }

let parse xml =
  Some
    { organizations_entity_path =
        Aws.Util.option_bind (Aws.Xml.member "OrganizationsEntityPath" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organizations_entity_path (fun f ->
             Aws.Query.Pair ("OrganizationsEntityPath", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organizations_entity_path (fun f ->
             "OrganizationsEntityPath", String.to_json f)
       ])

let of_json j =
  { organizations_entity_path =
      Aws.Util.option_map (Aws.Json.lookup j "OrganizationsEntityPath") String.of_json
  }
