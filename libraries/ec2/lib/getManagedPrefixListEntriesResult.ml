open Aws.BaseTypes

type t =
  { entries : PrefixListEntrySet.t
  ; next_token : String.t option
  }

let make ?(entries = []) ?next_token () = { entries; next_token }

let parse xml =
  Some
    { entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "entrySet" xml) PrefixListEntrySet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("EntrySet", PrefixListEntrySet.to_query v.entries))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("entrySet", PrefixListEntrySet.to_json v.entries)
       ])

let of_json j =
  { entries =
      PrefixListEntrySet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "entrySet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
