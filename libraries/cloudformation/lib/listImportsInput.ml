open Aws.BaseTypes

type t =
  { export_name : String.t
  ; next_token : String.t option
  }

let make ~export_name ?next_token () = { export_name; next_token }

let parse xml =
  Some
    { export_name =
        Aws.Xml.required
          "ExportName"
          (Aws.Util.option_bind (Aws.Xml.member "ExportName" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("ExportName", String.to_query v.export_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("ExportName", String.to_json v.export_name)
       ])

let of_json j =
  { export_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExportName"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
