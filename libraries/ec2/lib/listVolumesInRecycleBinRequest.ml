open Aws.BaseTypes

type t =
  { volume_ids : VolumeIdStringList.t
  ; dry_run : Boolean.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make ?(volume_ids = []) ?dry_run ?max_results ?next_token () =
  { volume_ids; dry_run; max_results; next_token }

let parse xml =
  Some
    { volume_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "VolumeId" xml) VolumeIdStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("VolumeId", VolumeIdStringList.to_query v.volume_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("VolumeId", VolumeIdStringList.to_json v.volume_ids)
       ])

let of_json j =
  { volume_ids =
      VolumeIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VolumeId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
