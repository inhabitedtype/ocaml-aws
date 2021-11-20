open OUnit2
open Aws_sdb

type config =
  { access_key : string
  ; secret_key : string
  ; region : string
  }

let ( @? ) = assert_bool

module type Runtime = sig
  type 'a m

  val run_request :
       region:string
    -> access_key:string
    -> secret_key:string
    -> ?token:string
    -> (module Aws.Call
          with type input = 'input
           and type output = 'output
           and type error = 'error)
    -> 'input
    -> [ `Ok of 'output | `Error of 'error Aws.Error.t ] m

  val un_m : 'a m -> 'a
end

let sign_v2_request
    ~access_key
    ~secret_key
    ~service:_
    ~region:_
    (meth, uri, _headers)
    ~now
    ~host =
  let open Aws in
  let amzdate = Time.date_time_iso8601 now in
  let query =
    Uri.add_query_params'
      uri
      [ "Timestamp", amzdate
      ; "AWSAccessKeyId", access_key
      ; "SignatureMethod", "HmacSHA256"
      ; "SignatureVersion", "2"
      ]
  in

  let params = Signing.encode_query (Uri.query query) in
  let canonical_uri = "/" in
  let string_to_sign =
    Request.string_of_meth meth ^ "\n" ^ host ^ "\n" ^ canonical_uri ^ "\n" ^ params
  in
  ( string_to_sign
  , Uri.pct_encode
    @@ Base64.encode_string
    @@ Signing.Hash.sha256 ~key:secret_key string_to_sign )

module TestSuite =
functor
  (Runtime : Runtime)
  ->
  struct
    let create_domain config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module CreateDomain)
             (Types.CreateDomainRequest.make ~domain_name:"ocaml-aws-domain-test" ())))

    let list_domains config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module ListDomains)
             (Types.ListDomainsRequest.make ~max_number_of_domains:2 ())))

    let delete_domain config () =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module DeleteDomain)
             (Types.DeleteDomainRequest.make ~domain_name:"ocaml-aws-domain-test" ())))

    let create_list_delete_test config _ =
      let create_request = create_domain config () in
      ("Create Domain returns successfully"
      @?
      match create_request with
      | `Ok _ -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let list_request = list_domains config () in
      ("List Domains returns successfully"
      @?
      match list_request with
      | `Ok resp ->
          Printf.printf
            "%s\n"
            (Yojson.Basic.to_string
               Types.ListDomainsResult.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let delete_request = delete_domain config () in
      "Delete Domain returns successfully"
      @?
      match delete_request with
      | `Ok _ -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let sign_v2_request_test _ =
      (* Example taken from AWS signing V2 documentation.
         https://docs.aws.amazon.com/general/latest/gr/signature-version-2.html
      *)
      let expected =
        ( "GET\n\
           elasticmapreduce.amazonaws.com\n\
           /\n\
           AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE&Action=DescribeJobFlows&SignatureMethod=HmacSHA256&SignatureVersion=2&Timestamp=2011-10-03T15%3A19%3A30&Version=2009-03-31"
        , "i91nKc4PWAt0JJIdXwz9HxZCJDdiy6cf%2FMj6vPxyYIs=" )
      in
      let host = "elasticmapreduce.amazonaws.com" in
      let uri =
        Uri.add_query_params'
          (Uri.of_string "elasticmapreduce.amazonaws.com")
          [ "Action", "DescribeJobFlows"; "Version", "2009-03-31" ]
      in

      let now =
        CalendarLib.Printer.Calendar.from_fstring
          "%Y-%m-%dT%H:%M:%S"
          "2011-10-03T15:19:30"
      in
      let cannonical_query_string, request =
        sign_v2_request
          ~access_key:"AKIAIOSFODNN7EXAMPLE"
          ~secret_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
          ~service:"elasticmapreduce"
          ~region:"ap-southeast-2"
          ~now
          ~host
          (`GET, uri, ())
      in
      assert_equal (cannonical_query_string, request) expected

    let suite config =
      "Test SDB"
      >::: [ "SDB create/list/delete" >:: create_list_delete_test config
           ; "SDB signing is correct" >:: sign_v2_request_test
           ]

    let () =
      let access_key =
        try Some (Unix.getenv "AWS_ACCESS_KEY_ID") with Not_found -> None
      in
      let secret_key =
        try Some (Unix.getenv "AWS_SECRET_ACCESS_KEY") with Not_found -> None
      in
      let region = try Some (Unix.getenv "AWS_DEFAULT_REGION") with Not_found -> None in

      match access_key, secret_key, region with
      | Some access_key, Some secret_key, Some region ->
          run_test_tt_main (suite { access_key; secret_key; region })
      | _, _, _ ->
          Printf.eprintf
            "Skipping running tests. Environment variables AWS_ACCESS_KEY_ID, \
             AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION not available. ";
          exit 0
  end
