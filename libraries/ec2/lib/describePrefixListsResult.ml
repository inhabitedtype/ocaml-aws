open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; prefix_lists : PrefixListSet.t
  }

let make ?next_token ?(prefix_lists = []) () = { next_token; prefix_lists }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; prefix_lists =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "prefixListSet" xml) PrefixListSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PrefixListSet", PrefixListSet.to_query v.prefix_lists))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("prefixListSet", PrefixListSet.to_json v.prefix_lists)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; prefix_lists =
      PrefixListSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "prefixListSet"))
  }
