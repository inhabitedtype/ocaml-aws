type 'a code =
  | Understood of 'a
  | Unknown of string

type bad_response =
  { body : string
  ; message : string
  }

type 'a error_response =
  | BadResponse of bad_response
  | AwsError of ('a code * string) list

type 'a t =
  | TransportError of string
  | HttpError of int * 'a error_response

let code_to_string utos = function
  | Understood code -> utos code
  | Unknown code -> code

let format print_native = function
  | TransportError msg -> Printf.(sprintf "TransportError %s" msg)
  | HttpError (code, err) -> (
      match err with
      | BadResponse br ->
          Printf.sprintf
            "HttpError(%d - BadResponse): %s. Body: %s\n"
            code
            br.message
            br.body
      | AwsError ers ->
          Printf.sprintf
            "HttpError(%d - AwsError): %s"
            code
            (String.concat
               ", "
               (List.map
                  (fun (code, msg) ->
                    Printf.sprintf "[%s: %s]" (code_to_string print_native code) msg)
                  ers)))

let parse_aws_error body =
  try
    let tags = Ezxmlm.from_string body |> snd in
    let errors =
      Util.(
        match
          option_bind (Xml.member "Response" tags) (fun r ->
              option_bind (Xml.member "Errors" r) (fun errs ->
                  Some (Xml.members "Error" errs)))
        with
        | Some es -> Some es
        | None ->
            option_bind (Xml.member "ErrorResponse" tags) (fun r ->
                Some (Xml.members "Error" r)))
    in
    match errors with
    | None -> `Error "Could not find <Error> nodes for error response code."
    | Some err_nodes ->
        Util.(
          option_map
            (List.map
               (fun node ->
                 match
                   ( option_map (Xml.member "Code" node) Xml.data_to_string
                   , option_map (Xml.member "Message" node) Xml.data_to_string )
                 with
                 | Some error_code, Some message -> Some (error_code, message)
                 | _ -> None)
               err_nodes
            |> option_all)
            (fun res -> `Ok res)
          |> of_option
               (`Error
                 "Could not find properly formatted <Error> nodes in <Errors> response."))
  with Failure msg -> `Error ("Error parsing xml: " ^ msg)
