type t =
  { successful : ResponseHostIdList.t
  ; unsuccessful : UnsuccessfulItemList.t
  }

let make ?(successful = []) ?(unsuccessful = []) () = { successful; unsuccessful }

let parse xml =
  Some
    { successful =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "successful" xml)
             ResponseHostIdList.parse)
    ; unsuccessful =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unsuccessful" xml)
             UnsuccessfulItemList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Unsuccessful", UnsuccessfulItemList.to_query v.unsuccessful))
       ; Some (Aws.Query.Pair ("Successful", ResponseHostIdList.to_query v.successful))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("unsuccessful", UnsuccessfulItemList.to_json v.unsuccessful)
       ; Some ("successful", ResponseHostIdList.to_json v.successful)
       ])

let of_json j =
  { successful =
      ResponseHostIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "successful"))
  ; unsuccessful =
      UnsuccessfulItemList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unsuccessful"))
  }
