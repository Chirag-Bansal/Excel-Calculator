let _ =
    try
    	let in_stream = open_in "inp.txt" in
	        let lexbuf = Lexing.from_channel in_stream in
		        while true do
		            let result = Parser.main Lexer.token lexbuf in
		            	Printf.printf "%s" result;
		    	        flush stdout
		        done;
		        close_in in_stream;
	with 
		| Lexer.Eof ->
	    	exit 0
  		| e ->
  			raise e;
		    exit 0