open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; volume_statuses : VolumeStatusList.t
  }

let make ?next_token ?(volume_statuses = []) () = { next_token; volume_statuses }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; volume_statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "volumeStatusSet" xml)
             VolumeStatusList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("VolumeStatusSet", VolumeStatusList.to_query v.volume_statuses))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("volumeStatusSet", VolumeStatusList.to_json v.volume_statuses)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; volume_statuses =
      VolumeStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeStatusSet"))
  }
