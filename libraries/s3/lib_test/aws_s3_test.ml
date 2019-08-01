open OUnit
open Aws_s3

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
      region:string
      -> (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct

  let create_bucket bucket region () =
    let res = Runtime.(un_m (run_request
                               ~region
                               (module CreateBucket)
                               (Types.CreateBucketRequest.make ~bucket ()))) in
    begin match res with
       | `Ok resp ->
          resp.location
       | `Error err -> begin
           Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); None end
       end

  let delete bucket region =
    let result = Runtime.(un_m (run_request
                                  ~region
                                  (module DeleteBucket)
                                  (Types.DeleteBucketRequest.make ~bucket ()))) in
    "Creating bucket and then deleting succeeds"
    @? begin match result with
       | `Ok _ -> true
       | `Error e -> begin print_endline (Aws.Error.format Errors_internal.to_string e); false end
       end

  let create () =
    let region = "us-east-1" in
    let bucket = "test-bucket" in
    let result = create_bucket bucket region () in
    "Creating bucket succeeds"
    @? begin match result with
       | Some bucket -> true
       | None -> false
       end;
    let bucket_name = match result with
      | Some bucket -> bucket
      | None -> assert false
    in
    delete bucket_name region

  let write_bucket region bucket key string () =
    let res = Runtime.(un_m (run_request
                               ~region
                               (module PutObject)
                               (Types.PutObjectRequest.make ~bucket:bucket ~key:key ~body:string ()))) in
    "Writing bucket succeeds"
    @? begin match res with
    | `Ok _ -> true
    | `Error e -> begin print_endline (Aws.Error.format Errors_internal.to_string e); false end
    end

  let read_bucket region bucket key () =
    let res = Runtime.(un_m (run_request
                               ~region
                               (module GetObject)
                               (Types.GetObjectRequest.make ~bucket:bucket ~key:key ()))) in
    "Reading bucket succeeds"
    @? begin match res with
       | `Ok r -> begin match r.body with
                  | Some s -> true
                  | None -> begin
                      print_endline ("No value read back from bucket: " ^ bucket ^ " key: " ^ key);
                      false
                    end
                  end
       | `Error e -> begin print_endline (Aws.Error.format Errors_internal.to_string e); false end
       end

  let create_write_read () =
    let region = "us-east-1" in
    let bucket = "test-bucket" in
    let result = create_bucket bucket region () in
    "Creating bucket succeeds"
    @? begin match result with
       | Some bucket -> true
       | None -> false
       end;
    let bucket_name = match result with
      | Some bucket -> bucket
      | None -> assert false
    in
    write_bucket region bucket_name "test_string" "test_key" ();
    read_bucket region bucket_name "test_key" ();
    delete bucket_name region

  let test_cases =
    [ "Bucket Create / Delete" >:: create
    ; "Bucket Create / Write / Read" >:: create_write_read
    ]

  let rec was_successful =
    function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
      was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
      false
  let _ =
    let suite = "Tests" >::: test_cases in
    let verbose = ref false in
    let set_verbose _ = verbose := true in
    Arg.parse
      [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
    if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
      exit 1
end
