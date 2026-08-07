type t = { statuses : IdFormatList.t }

let make ?(statuses = []) () = { statuses }

let parse xml =
  Some
    { statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "statusSet" xml) IdFormatList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("StatusSet", IdFormatList.to_query v.statuses)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("statusSet", IdFormatList.to_json v.statuses) ])

let of_json j =
  { statuses =
      IdFormatList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "statusSet"))
  }
