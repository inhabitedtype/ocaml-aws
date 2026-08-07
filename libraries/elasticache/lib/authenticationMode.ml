type t =
  { type_ : InputAuthenticationType.t option
  ; passwords : PasswordListInput.t
  }

let make ?type_ ?(passwords = []) () = { type_; passwords }

let parse xml =
  Some
    { type_ =
        Aws.Util.option_bind (Aws.Xml.member "Type" xml) InputAuthenticationType.parse
    ; passwords =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Passwords" xml) PasswordListInput.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Passwords.member", PasswordListInput.to_query v.passwords))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", InputAuthenticationType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Passwords", PasswordListInput.to_json v.passwords)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", InputAuthenticationType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") InputAuthenticationType.of_json
  ; passwords =
      PasswordListInput.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Passwords"))
  }
