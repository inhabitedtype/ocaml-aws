open Aws.BaseTypes

type t =
  { image_id : String.t
  ; delete_associated_snapshots : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make ~image_id ?delete_associated_snapshots ?dry_run () =
  { image_id; delete_associated_snapshots; dry_run }

let parse xml =
  Some
    { image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; delete_associated_snapshots =
        Aws.Util.option_bind
          (Aws.Xml.member "DeleteAssociatedSnapshots" xml)
          Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.delete_associated_snapshots (fun f ->
             Aws.Query.Pair ("DeleteAssociatedSnapshots", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.delete_associated_snapshots (fun f ->
             "DeleteAssociatedSnapshots", Boolean.to_json f)
       ; Some ("ImageId", String.to_json v.image_id)
       ])

let of_json j =
  { image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; delete_associated_snapshots =
      Aws.Util.option_map (Aws.Json.lookup j "DeleteAssociatedSnapshots") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
