open OUnit
open Aws_sdb

module type Runtime = sig
  type 'a m

  val run_request :
       (module Aws.Call
          with type input = 'input
           and type output = 'output
           and type error = 'error)
    -> 'input
    -> [ `Ok of 'output | `Error of 'error Aws.Error.t ] m

  val un_m : 'a m -> 'a
end

let sign_v2_request ~access_key ~secret_key ~service ~region (meth, uri, headers) ~now ~host =
  let open Aws in
  let amzdate = Time.date_time_iso8601 now in
  let query = Uri.add_query_params' uri
                [ "Timestamp", amzdate
                ; "AWSAccessKeyId", access_key
                ; "SignatureMethod", "HmacSHA256"
                ; "SignatureVersion", "2"
                ] in

  let params = Signing.encode_query (Uri.query query) in
  let canonical_uri = "/" in
  let string_to_sign =
    Request.string_of_meth meth
    ^ "\n"
    ^ host
    ^ "\n"
    ^ canonical_uri
    ^ "\n"
    ^ params in
  (string_to_sign,  Uri.pct_encode @@ Base64.encode_string @@ Signing.Hash.sha256 ~key:secret_key string_to_sign)

module TestSuite =
functor
  (Runtime : Runtime)
  ->
  struct
    let create_domain () =
      Runtime.(
        un_m
          (run_request
             (module CreateDomain)
             (Types.CreateDomainRequest.make ~domain_name:"ocaml-aws-domain-test" ())))

    let list_domains () =
      Runtime.(
        un_m
          (run_request
             (module ListDomains)
             (Types.ListDomainsRequest.make ~max_number_of_domains:2 ())))

    let delete_domain () =
      Runtime.(
        un_m
          (run_request
             (module DeleteDomain)
             (Types.DeleteDomainRequest.make ~domain_name:"ocaml-aws-domain-test" ())))

    let create_list_delete_test () =
      let create_request = create_domain () in
      ("Create Domain returns successfully"
      @?
      match create_request with
      | `Ok resp -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let list_request = list_domains () in
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
      let delete_request = delete_domain () in
      "Delete Domain returns successfully"
      @?
      match delete_request with
      | `Ok resp -> true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let sign_v2_request_test () =
      (* Example taken from AWS signing V2 documentation.
         https://docs.aws.amazon.com/general/latest/gr/signature-version-2.html
       *)
      let expected = ("GET\nelasticmapreduce.amazonaws.com\n/\nAWSAccessKeyId=AKIAIOSFODNN7EXAMPLE&Action=DescribeJobFlows&SignatureMethod=HmacSHA256&SignatureVersion=2&Timestamp=2011-10-03T15%3A19%3A30&Version=2009-03-31"
                     , "i91nKc4PWAt0JJIdXwz9HxZCJDdiy6cf%2FMj6vPxyYIs=") in
      let host = "elasticmapreduce.amazonaws.com" in
      let uri = Uri.add_query_params' (Uri.of_string "elasticmapreduce.amazonaws.com")
                  ["Action", "DescribeJobFlows";
                   "Version", "2009-03-31"]in

      let now = CalendarLib.Printer.Calendar.from_fstring "%Y-%m-%dT%H:%M:%S" "2011-10-03T15:19:30" in
      let (cannonical_query_string, request) = sign_v2_request
                                                 ~access_key:"AKIAIOSFODNN7EXAMPLE"
                                                 ~secret_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
                                                 ~service:"elasticmapreduce" ~region:"ap-southeast-2"
                                                 ~now ~host (`GET, uri, ()) in
      assert_equal (cannonical_query_string,  request) expected


    let test_cases = [ "SDB create/list/delete" >:: create_list_delete_test
                     ; "SDB signing is correct" >:: sign_v2_request_test]

    let rec was_successful = function
      | [] -> true
      | RSuccess _ :: t | RSkip _ :: t -> was_successful t
      | RFailure _ :: _ | RError _ :: _ | RTodo _ :: _ -> false

    let _ =
      let suite = "Tests" >::: test_cases in
      let verbose = ref false in
      let set_verbose _ = verbose := true in
      Arg.parse
        [ "-verbose", Arg.Unit set_verbose, "Run the test in verbose mode." ]
        (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
        ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
      if not (was_successful (run_test_tt ~verbose:!verbose suite)) then exit 1
  end
