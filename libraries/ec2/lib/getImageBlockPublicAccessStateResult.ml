open Aws.BaseTypes

type t =
  { image_block_public_access_state : String.t option
  ; managed_by : ManagedBy.t option
  }

let make ?image_block_public_access_state ?managed_by () =
  { image_block_public_access_state; managed_by }

let parse xml =
  Some
    { image_block_public_access_state =
        Aws.Util.option_bind
          (Aws.Xml.member "imageBlockPublicAccessState" xml)
          String.parse
    ; managed_by = Aws.Util.option_bind (Aws.Xml.member "managedBy" xml) ManagedBy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f ->
             Aws.Query.Pair ("ManagedBy", ManagedBy.to_query f))
       ; Aws.Util.option_map v.image_block_public_access_state (fun f ->
             Aws.Query.Pair ("ImageBlockPublicAccessState", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f -> "managedBy", ManagedBy.to_json f)
       ; Aws.Util.option_map v.image_block_public_access_state (fun f ->
             "imageBlockPublicAccessState", String.to_json f)
       ])

let of_json j =
  { image_block_public_access_state =
      Aws.Util.option_map (Aws.Json.lookup j "imageBlockPublicAccessState") String.of_json
  ; managed_by = Aws.Util.option_map (Aws.Json.lookup j "managedBy") ManagedBy.of_json
  }
