structure Test =  struct


  (* start up a server on the given port of the machine.
   * connect t   o the port using 'telnet' 
   *)	   

  fun test p = let
      fun f s = let 
	  (* open socket s, and get instream for reading and outstream for writing*)
	  val (is,os) = SocketIO.openSocket s
	  fun loop () = let 
	      (* print a prompt on the socket *)
	      val _ = SocketIO.output (os, "> ")
	  in
	      (* read a line from the socket *)
	      case SocketIO.inputLine is
	       of NONE => ()
		| SOME "quit\r\n" => ()
		| SOME str => 
                    (* write the input size + a list of characters of the input
		     * to the socket    *)
		    (SocketIO.output (os, (Int.toString (size str))^"\n");
		     app (fn (c) => SocketIO.output (os,concat ["[",Char.toString(c),"]"]))
			 (String.explode str);
		     SocketIO.output (os,"\n");
		     loop ())
	  end
      in
	  loop ();
	  Socket.close s
      end
  in
      (* create a server on the given port, and provide function f as a callback *)
      Server.mkSingleServer p f
  end
		 
end
