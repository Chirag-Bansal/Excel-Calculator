%{
open Functions
open Array
open Str

let m = 6;;
let n = 3;;

let x = make_matrix m n Nothing;;

try
  let in_stream = open_in "in.txt" in
      for i=0 to (m-1) do
        let line = input_line in_stream in
        let split = String.split_on_char ',' in
        let values = split line in
        let values_array = of_list values in
          for j = 0 to n-1 do
            if(values_array.(j) = "") then
              x.(i).(j) <- Nothing
            else
              x.(i).(j) <- Float (float_of_string (values_array.(j)))
          done;
      done;
      print_array x;
      Printf.printf "\n";
      close_in in_stream; 
  with e ->
    raise e

%}

%token <int> INT
%token <float> FLOAT
%token PROPEN PRCLOSE BROPEN BRCLOSE COMMA COLON
%token <string> RANGES
%token <string> INDICES
%token ASSIGN TERM COUNT ROWCOUNT COLCOUNT SUM ROWSUM COLSUM
%token AVG ROWAVG COLAVG MIN ROWMIN COLMIN MAX ROWMAX COLMAX
%token ADD SUBT MULT DIV EOL      
%start main             
%type <string> main

%%

main:
  expr EOL  {""}
;

expr:
  | INDICES {}
  | FLOAT {}
  | INDICES ASSIGN COUNT RANGES TERM {
                                      full_count x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"                                  
                                    }
  | INDICES ASSIGN ROWCOUNT RANGES TERM {
                                      row_count x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN COLCOUNT RANGES TERM {
                                      col_count x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN AVG RANGES TERM {
                                      full_avg x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN ROWAVG RANGES TERM {
                                      row_avg x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN COLAVG RANGES TERM {
                                      col_avg x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN SUM RANGES TERM {
                                      full_sum x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN COLSUM RANGES TERM {
                                      col_sum x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN ROWSUM RANGES TERM {
                                      row_sum x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN MIN RANGES TERM {
                                      full_min x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN ROWMIN RANGES TERM {
                                      row_min x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN COLMIN RANGES TERM {
                                      col_min x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN MAX RANGES TERM {
                                      full_max x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN ROWMAX RANGES TERM {
                                      row_max x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN COLMAX RANGES TERM {
                                      col_max x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN ADD RANGES RANGES TERM {
                                      add_range x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN ADD INDICES RANGES TERM {
                                      add_const x (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) (value x.(int_of_string(get_first_index $4)).(int_of_string(get_second_index $4)));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN ADD RANGES INDICES TERM {
                                      add_const x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) (value x.(int_of_string(get_first_index $5)).(int_of_string(get_second_index $5)));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN ADD FLOAT RANGES TERM {
                                      add_const x (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) $4;
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN ADD RANGES FLOAT TERM {
                                      add_const x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) $5;
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN SUBT RANGES RANGES TERM {
                                      subt_range x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN SUBT INDICES RANGES TERM {
                                      sub_const x (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) (value x.(int_of_string(get_first_index $4)).(int_of_string(get_second_index $4)));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN SUBT RANGES INDICES TERM  {
                                      sub_const x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) (value x.(int_of_string(get_first_index $5)).(int_of_string(get_second_index $5)));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN SUBT FLOAT RANGES TERM {
                                      sub_const x (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) $4;
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN SUBT RANGES FLOAT TERM {
                                      sub_const x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) $5;
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN MULT RANGES RANGES TERM {
                                      mult_range x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN MULT INDICES RANGES TERM {
                                      mult_const x (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) (value x.(int_of_string(get_first_index $4)).(int_of_string(get_second_index $4)));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN MULT RANGES INDICES TERM  {
                                      mult_const x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) (value x.(int_of_string(get_first_index $5)).(int_of_string(get_second_index $5)));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN MULT FLOAT RANGES TERM {
                                      mult_const x (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) $4;
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN MULT RANGES FLOAT TERM {
                                      mult_const x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) $5;
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN DIV RANGES RANGES TERM {
                                      div_range x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN DIV INDICES RANGES TERM {
                                      div_const x (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) (value x.(int_of_string(get_first_index $4)).(int_of_string(get_second_index $4)));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN DIV RANGES INDICES TERM  {
                                      div_const x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) (value x.(int_of_string(get_first_index $5)).(int_of_string(get_second_index $5)));
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN DIV FLOAT RANGES TERM {
                                      div_const x (int_of_string(get_first_row_range $5)) (int_of_string(get_second_row_range $5)) (int_of_string(get_first_col_range $5)) (int_of_string(get_second_col_range $5)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) $4;
                                      print_array x;
                                      Printf.printf "\n"
                                    }
  | INDICES ASSIGN DIV RANGES FLOAT TERM {
                                      div_const x (int_of_string(get_first_row_range $4)) (int_of_string(get_second_row_range $4)) (int_of_string(get_first_col_range $4)) (int_of_string(get_second_col_range $4)) (int_of_string(get_first_index $1)) (int_of_string(get_second_index $1)) $5;
                                      print_array x;
                                      Printf.printf "\n"
                                    }
;