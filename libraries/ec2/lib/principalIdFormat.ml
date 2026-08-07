open Aws.BaseTypes

type t =
  { arn : String.t option
  ; statuses : IdFormatList.t
  }

let make ?arn ?(statuses = []) () = { arn; statuses }

let parse xml =
  Some
    { arn = Aws.Util.option_bind (Aws.Xml.member "arn" xml) String.parse
    ; statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "statusSet" xml) IdFormatList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("StatusSet", IdFormatList.to_query v.statuses))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("statusSet", IdFormatList.to_json v.statuses)
       ; Aws.Util.option_map v.arn (fun f -> "arn", String.to_json f)
       ])

let of_json j =
  { arn = Aws.Util.option_map (Aws.Json.lookup j "arn") String.of_json
  ; statuses =
      IdFormatList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "statusSet"))
  }
