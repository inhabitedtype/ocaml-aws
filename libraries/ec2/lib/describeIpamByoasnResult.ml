open Aws.BaseTypes

type t =
  { byoasns : ByoasnSet.t
  ; next_token : String.t option
  }

let make ?(byoasns = []) ?next_token () = { byoasns; next_token }

let parse xml =
  Some
    { byoasns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "byoasnSet" xml) ByoasnSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("ByoasnSet", ByoasnSet.to_query v.byoasns))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("byoasnSet", ByoasnSet.to_json v.byoasns)
       ])

let of_json j =
  { byoasns = ByoasnSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "byoasnSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
