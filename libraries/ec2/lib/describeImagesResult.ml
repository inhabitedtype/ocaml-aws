open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; images : ImageList.t
  }

let make ?next_token ?(images = []) () = { next_token; images }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; images =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "imagesSet" xml) ImageList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ImagesSet", ImageList.to_query v.images))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("imagesSet", ImageList.to_json v.images)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; images = ImageList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "imagesSet"))
  }
