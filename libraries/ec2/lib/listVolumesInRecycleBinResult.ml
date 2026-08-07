open Aws.BaseTypes

type t =
  { volumes : VolumeRecycleBinInfoList.t
  ; next_token : String.t option
  }

let make ?(volumes = []) ?next_token () = { volumes; next_token }

let parse xml =
  Some
    { volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "volumeSet" xml)
             VolumeRecycleBinInfoList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("VolumeSet", VolumeRecycleBinInfoList.to_query v.volumes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("volumeSet", VolumeRecycleBinInfoList.to_json v.volumes)
       ])

let of_json j =
  { volumes =
      VolumeRecycleBinInfoList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
