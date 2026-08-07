open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; volumes_modifications : VolumeModificationList.t
  }

let make ?next_token ?(volumes_modifications = []) () =
  { next_token; volumes_modifications }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; volumes_modifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "volumeModificationSet" xml)
             VolumeModificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "VolumeModificationSet"
              , VolumeModificationList.to_query v.volumes_modifications ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "volumeModificationSet"
           , VolumeModificationList.to_json v.volumes_modifications )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; volumes_modifications =
      VolumeModificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeModificationSet"))
  }
