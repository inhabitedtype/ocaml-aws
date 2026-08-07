open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; image_references : ImageReferenceList.t
  }

let make ?next_token ?(image_references = []) () = { next_token; image_references }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; image_references =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "imageReferenceSet" xml)
             ImageReferenceList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ImageReferenceSet", ImageReferenceList.to_query v.image_references))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("imageReferenceSet", ImageReferenceList.to_json v.image_references)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; image_references =
      ImageReferenceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "imageReferenceSet"))
  }
