open Aws.BaseTypes

type t =
  { stacks : Stacks.t
  ; next_token : String.t option
  }

let make ?(stacks = []) ?next_token () = { stacks; next_token }

let parse xml =
  Some
    { stacks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Stacks" xml) Stacks.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Stacks.member", Stacks.to_query v.stacks))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Stacks", Stacks.to_json v.stacks)
       ])

let of_json j =
  { stacks = Stacks.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Stacks"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
