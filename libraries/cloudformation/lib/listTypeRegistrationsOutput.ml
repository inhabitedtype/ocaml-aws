open Aws.BaseTypes

type t =
  { registration_token_list : RegistrationTokenList.t
  ; next_token : String.t option
  }

let make ?(registration_token_list = []) ?next_token () =
  { registration_token_list; next_token }

let parse xml =
  Some
    { registration_token_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RegistrationTokenList" xml)
             RegistrationTokenList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RegistrationTokenList.member"
              , RegistrationTokenList.to_query v.registration_token_list ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "RegistrationTokenList"
           , RegistrationTokenList.to_json v.registration_token_list )
       ])

let of_json j =
  { registration_token_list =
      RegistrationTokenList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RegistrationTokenList"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
