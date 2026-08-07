type t = { unsuccessful : UnsuccessfulItemSet.t }

let make ?(unsuccessful = []) () = { unsuccessful }

let parse xml =
  Some
    { unsuccessful =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unsuccessful" xml)
             UnsuccessfulItemSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Unsuccessful", UnsuccessfulItemSet.to_query v.unsuccessful))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("unsuccessful", UnsuccessfulItemSet.to_json v.unsuccessful) ])

let of_json j =
  { unsuccessful =
      UnsuccessfulItemSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unsuccessful"))
  }
