type t = { vpc_attachment : VpcAttachment.t option }

let make ?vpc_attachment () = { vpc_attachment }

let parse xml =
  Some
    { vpc_attachment =
        Aws.Util.option_bind (Aws.Xml.member "attachment" xml) VpcAttachment.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_attachment (fun f ->
             Aws.Query.Pair ("Attachment", VpcAttachment.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_attachment (fun f ->
             "attachment", VpcAttachment.to_json f)
       ])

let of_json j =
  { vpc_attachment =
      Aws.Util.option_map (Aws.Json.lookup j "attachment") VpcAttachment.of_json
  }
