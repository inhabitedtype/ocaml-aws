(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Lwt.Infix

let run_request
    (type input output error)
    ~region
    ~access_key
    ~secret_key
    ?token
    (module M : Aws.Call
      with type input = input
       and type output = output
       and type error = error)
    (inp : M.input) =
  let meth, uri, headers =
    match M.signature_version with
    | V4 | S3 ->
        Aws.Signing.sign_request
          ~access_key
          ~secret_key
          ?token
          ~service:M.service
          ~region
          (M.to_http M.service region inp)
    | V2 ->
        Aws.Signing.sign_v2_request
          ~access_key
          ~secret_key
          ?token
          ~service:M.service
          ~region
          (M.to_http M.service region inp)
  in
  let open Cohttp in
  let headers = Header.of_list headers in
  Lwt.catch
    (fun () ->
      Cohttp_lwt_unix.Client.call ~headers meth uri
      >>= fun (resp, body) ->
      Cohttp_lwt.Body.to_string body
      >|= fun body ->
      let code = Code.code_of_status (Response.status resp) in
      if code >= 300
      then
        let open Aws.Error in
        let aws_error =
          match parse_aws_error body with
          | `Error message -> BadResponse { body; message }
          | `Ok ers ->
              AwsError
                (List.map
                   (fun (aws_code, message) ->
                     match M.parse_error code aws_code with
                     | None -> Unknown aws_code, message
                     | Some e -> Understood e, message)
                   ers)
        in
        `Error (HttpError (code, aws_error))
      else
        match M.of_http body with
        | `Ok v -> `Ok v
        | `Error t -> `Error (Aws.Error.HttpError (code, t)))
    (function
      | Failure msg -> Lwt.return (`Error (Aws.Error.TransportError msg))
      | exn -> Lwt.fail exn)
