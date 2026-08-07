open Aws.BaseTypes

type t =
  { collection_id : String.t
  ; next_token : String.t option
  ; max_results : String.t option
  }

let make ~collection_id ?next_token ?max_results () =
  { collection_id; next_token; max_results }

let parse xml =
  Some
    { collection_id =
        Aws.Xml.required
          "CidrCollectionId"
          (Aws.Util.option_bind (Aws.Xml.member "CidrCollectionId" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nexttoken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxresults" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("maxresults", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("nexttoken", String.to_query f))
       ; Some (Aws.Query.Pair ("CidrCollectionId", String.to_query v.collection_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "maxresults", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nexttoken", String.to_json f)
       ; Some ("CidrCollectionId", String.to_json v.collection_id)
       ])

let of_json j =
  { collection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CidrCollectionId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nexttoken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxresults") String.of_json
  }
