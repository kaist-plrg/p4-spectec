open Linol_eio

let publish ~position_encoding ~(notify_back : Jsonrpc2.notify_back) uri text =
  notify_back#set_uri uri;
  notify_back#send_diagnostic
    (Check.run ~position_encoding ~path:(DocumentUri.to_path uri) text)

let server =
  object
    inherit Jsonrpc2.server

    (* The diagnostic collector is shared across checks. *)
    method spawn_query_handler f = f ()

    method on_notif_doc_did_open ~notify_back (doc : TextDocumentItem.t)
        ~content =
      publish ~position_encoding:positionEncoding ~notify_back doc.uri content

    method on_notif_doc_did_change ~notify_back
        (doc : VersionedTextDocumentIdentifier.t) _changes ~old_content:_
        ~new_content =
      publish ~position_encoding:positionEncoding ~notify_back doc.uri
        new_content

    method on_notif_doc_did_close ~notify_back (doc : TextDocumentIdentifier.t)
        =
      Hashtbl.remove docs doc.uri;
      notify_back#set_uri doc.uri;
      notify_back#send_diagnostic []
  end

let serve () =
  Eio_main.run @@ fun env -> Jsonrpc2.run (Jsonrpc2.create_stdio ~env server)
