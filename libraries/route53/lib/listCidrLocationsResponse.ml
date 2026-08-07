open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; cidr_locations : LocationSummaries.t
  }

let make ?next_token ?(cidr_locations = []) () = { next_token; cidr_locations }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; cidr_locations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CidrLocations" xml)
             LocationSummaries.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("CidrLocations.member", LocationSummaries.to_query v.cidr_locations))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CidrLocations", LocationSummaries.to_json v.cidr_locations)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; cidr_locations =
      LocationSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CidrLocations"))
  }
