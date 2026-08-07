open Aws.BaseTypes

type t = { registration_token : String.t }

let make ~registration_token () = { registration_token }

let parse xml =
  Some
    { registration_token =
        Aws.Xml.required
          "RegistrationToken"
          (Aws.Util.option_bind (Aws.Xml.member "RegistrationToken" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("RegistrationToken", String.to_query v.registration_token))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("RegistrationToken", String.to_json v.registration_token) ])

let of_json j =
  { registration_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RegistrationToken"))
  }
