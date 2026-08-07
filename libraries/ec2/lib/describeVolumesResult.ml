open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; volumes : VolumeList.t
  }

let make ?next_token ?(volumes = []) () = { next_token; volumes }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "volumeSet" xml) VolumeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VolumeSet", VolumeList.to_query v.volumes))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("volumeSet", VolumeList.to_json v.volumes)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; volumes = VolumeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeSet"))
  }
