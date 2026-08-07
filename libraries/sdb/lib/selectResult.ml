open Aws.BaseTypes

type t =
  { items : ItemList.t
  ; next_token : String.t option
  }

let make ?(items = []) ?next_token () = { items; next_token }

let parse xml =
  Some
    { items = Aws.Util.of_option [] (ItemList.parse xml)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Items.member", ItemList.to_query v.items))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Items", ItemList.to_json v.items)
       ])

let of_json j =
  { items = ItemList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Items"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
