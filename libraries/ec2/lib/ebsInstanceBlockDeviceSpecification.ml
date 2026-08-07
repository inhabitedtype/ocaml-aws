open Aws.BaseTypes

type t =
  { volume_id : String.t option
  ; delete_on_termination : Boolean.t option
  }

let make ?volume_id ?delete_on_termination () = { volume_id; delete_on_termination }

let parse xml =
  Some
    { volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    ; delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ; Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_on_termination (fun f ->
             "deleteOnTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ])

let of_json j =
  { volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  ; delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "deleteOnTermination") Boolean.of_json
  }
