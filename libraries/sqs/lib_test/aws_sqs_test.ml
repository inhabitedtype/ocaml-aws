open OUnit2
open Aws_sqs

let from_opt = function
  | None -> OUnit.assert_failure "from_opt is None!"
  | Some x -> x

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

module TestSuite =
functor
  (Runtime : Runtime)
  ->
  struct
    let delete_queue config queue_url =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module DeleteQueue)
             (Types.DeleteQueueRequest.make ~queue_url ())))

    (* TODO Tag created queues with project name for later cleanup / tracking *)
    let create_queue config queue_name =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module CreateQueue)
             (Types.CreateQueueRequest.make ~queue_name ())))

    let send_message config queue_url message_body =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module SendMessage)
             (Types.SendMessageRequest.make ~queue_url ~message_body ())))

    let receive_message config queue_url =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module ReceiveMessage)
             (Types.ReceiveMessageRequest.make ~queue_url ())))

    let list_queues config ~queue_name_prefix =
      Runtime.(
        un_m
          (run_request
             ~region:config.region
             ~access_key:config.access_key
             ~secret_key:config.secret_key
             (module ListQueues)
             (Types.ListQueuesRequest.make ~queue_name_prefix ())))

    let print_messages (msg : Types.MessageList.t) =
      List.iter
        (function
          | (x : Types.Message.t) -> Printf.printf "Message: %s\n" (from_opt x.body))
        msg

    let send_receive_message_test config _ =
      let test_message = "Information about the largest city in Any Region." in
      let queue_name = "aws-sqs-test-2" in
      let create_res = create_queue config queue_name in
      ("SQS create queue"
      @?
      match create_res with
      | `Ok resp ->
          Printf.printf
            "Queue created %s\n"
            (Yojson.Basic.to_string
               Types.CreateQueueResult.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf
            "Create Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          false);

      let queue_url =
        match create_res with
        | `Ok resp -> from_opt resp.queue_url
        | `Error err ->
            Printf.printf
              "Create Error: %s\n"
              (Aws.Error.format Errors_internal.to_string err);
            OUnit.assert_failure "no queue_url for you!"
      in
      let send_message = send_message config queue_url test_message in
      ("SQS send message"
      @?
      match send_message with
      | `Ok _ ->
          Printf.printf "Sending message %s\n" test_message;
          true
      | `Error err ->
          Printf.printf
            "Sending Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          false);

      let receive_message = receive_message config queue_url in
      ("SQS receive message"
      @?
      match receive_message with
      | `Ok resp ->
          Printf.printf "Receiving messages\n";
          print_messages resp.messages;
          true
      | `Error err ->
          Printf.printf
            "Receiving Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          false);

      let delete_res = delete_queue config queue_url in
      "Delete queue"
      @?
      match delete_res with
      | `Ok _ ->
          Printf.printf "Queue %s deleted\n" queue_name;
          true
      | `Error err ->
          Printf.printf
            "Delete Error: %s\n"
            (Aws.Error.format Errors_internal.to_string err);
          false

    let create_delete_queue_test config _ =
      let queue_name = "aws-sqs-test-1" in
      let create_res = create_queue config queue_name in
      ("SQS create / delete queue"
      @?
      match create_res with
      | `Ok resp ->
          Printf.printf
            "%s\n"
            (Yojson.Basic.to_string
               Types.CreateQueueResult.(to_json (of_json (to_json resp))));
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false);
      let queue_url =
        match create_res with
        | `Ok resp -> from_opt resp.queue_url
        | `Error _ -> assert false
      in
      let delete_res = delete_queue config queue_url in
      "Delete queue"
      @?
      match delete_res with
      | `Ok _ ->
          Printf.printf "Queue %s deleted\n" queue_name;
          true
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let create_list_queue_test config _ =
      let queue_name_prefix = "aws-sqs-test-0" in
      let _ = create_queue config queue_name_prefix in
      let list_queue_res = list_queues config ~queue_name_prefix in
      "SQS create many queues and list"
      @?
      match list_queue_res with
      | `Ok resp ->
          Printf.printf
            "List queues: %s\n"
            (Yojson.Basic.to_string (Types.QueueUrlList.to_json resp.queue_urls));
          let _ = List.map (delete_queue config) resp.queue_urls in
          List.length resp.queue_urls == 1
      | `Error err ->
          Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
          false

    let suite config =
      "Test SQS"
      >::: [ "SQS create / delete queue" >:: create_delete_queue_test config
           ; "SQS create / list / delete queue" >:: create_list_queue_test config
           ; "SQS send / receive message" >:: send_receive_message_test config
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
