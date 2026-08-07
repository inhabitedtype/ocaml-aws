open Aws.BaseTypes

type t =
  { images : ImageRecycleBinInfoList.t
  ; next_token : String.t option
  }

let make ?(images = []) ?next_token () = { images; next_token }

let parse xml =
  Some
    { images =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "imageSet" xml)
             ImageRecycleBinInfoList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("ImageSet", ImageRecycleBinInfoList.to_query v.images))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("imageSet", ImageRecycleBinInfoList.to_json v.images)
       ])

let of_json j =
  { images =
      ImageRecycleBinInfoList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "imageSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
