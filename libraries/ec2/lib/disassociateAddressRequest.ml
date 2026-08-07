open Aws.BaseTypes

type t =
  { association_id : String.t option
  ; public_ip : String.t option
  ; dry_run : Boolean.t option
  }

let make ?association_id ?public_ip ?dry_run () = { association_id; public_ip; dry_run }

let parse xml =
  Some
    { association_id =
        Aws.Util.option_bind (Aws.Xml.member "AssociationId" xml) String.parse
    ; public_ip = Aws.Util.option_bind (Aws.Xml.member "PublicIp" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.public_ip (fun f -> "PublicIp", String.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "AssociationId", String.to_json f)
       ])

let of_json j =
  { association_id =
      Aws.Util.option_map (Aws.Json.lookup j "AssociationId") String.of_json
  ; public_ip = Aws.Util.option_map (Aws.Json.lookup j "PublicIp") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
