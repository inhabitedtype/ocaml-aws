open Aws.BaseTypes

type t =
  { verified_access_group_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~verified_access_group_id ?dry_run () = { verified_access_group_id; dry_run }

let parse xml =
  Some
    { verified_access_group_id =
        Aws.Xml.required
          "VerifiedAccessGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "VerifiedAccessGroupId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VerifiedAccessGroupId", String.to_query v.verified_access_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("VerifiedAccessGroupId", String.to_json v.verified_access_group_id)
       ])

let of_json j =
  { verified_access_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VerifiedAccessGroupId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
