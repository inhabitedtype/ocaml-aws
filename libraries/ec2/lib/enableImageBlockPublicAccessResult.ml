type t = { image_block_public_access_state : ImageBlockPublicAccessEnabledState.t option }

let make ?image_block_public_access_state () = { image_block_public_access_state }

let parse xml =
  Some
    { image_block_public_access_state =
        Aws.Util.option_bind
          (Aws.Xml.member "imageBlockPublicAccessState" xml)
          ImageBlockPublicAccessEnabledState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.image_block_public_access_state (fun f ->
             Aws.Query.Pair
               ( "ImageBlockPublicAccessState"
               , ImageBlockPublicAccessEnabledState.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.image_block_public_access_state (fun f ->
             "imageBlockPublicAccessState", ImageBlockPublicAccessEnabledState.to_json f)
       ])

let of_json j =
  { image_block_public_access_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "imageBlockPublicAccessState")
        ImageBlockPublicAccessEnabledState.of_json
  }
