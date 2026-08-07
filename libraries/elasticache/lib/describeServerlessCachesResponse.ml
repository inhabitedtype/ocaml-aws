open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; serverless_caches : ServerlessCacheList.t
  }

let make ?next_token ?(serverless_caches = []) () = { next_token; serverless_caches }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; serverless_caches =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ServerlessCaches" xml)
             ServerlessCacheList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ServerlessCaches.member", ServerlessCacheList.to_query v.serverless_caches))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ServerlessCaches", ServerlessCacheList.to_json v.serverless_caches)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; serverless_caches =
      ServerlessCacheList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessCaches"))
  }
