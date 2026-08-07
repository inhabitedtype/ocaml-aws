open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; cidr_collections : CollectionSummaries.t
  }

let make ?next_token ?(cidr_collections = []) () = { next_token; cidr_collections }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; cidr_collections =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CidrCollections" xml)
             CollectionSummaries.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("CidrCollections.member", CollectionSummaries.to_query v.cidr_collections))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CidrCollections", CollectionSummaries.to_json v.cidr_collections)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; cidr_collections =
      CollectionSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CidrCollections"))
  }
