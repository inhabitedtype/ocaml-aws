open Aws.BaseTypes

type t =
  { association_id : String.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  }

let make ~association_id ?client_token ?dry_run () =
  { association_id; client_token; dry_run }

let parse xml =
  Some
    { association_id =
        Aws.Xml.required
          "AssociationId"
          (Aws.Util.option_bind (Aws.Xml.member "AssociationId" xml) String.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some (Aws.Query.Pair ("AssociationId", String.to_query v.association_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("AssociationId", String.to_json v.association_id)
       ])

let of_json j =
  { association_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AssociationId"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
