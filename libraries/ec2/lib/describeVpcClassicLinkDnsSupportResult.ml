open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; vpcs : ClassicLinkDnsSupportList.t
  }

let make ?next_token ?(vpcs = []) () = { next_token; vpcs }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; vpcs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "vpcs" xml)
             ClassicLinkDnsSupportList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Vpcs", ClassicLinkDnsSupportList.to_query v.vpcs))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("vpcs", ClassicLinkDnsSupportList.to_json v.vpcs)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; vpcs =
      ClassicLinkDnsSupportList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcs"))
  }
