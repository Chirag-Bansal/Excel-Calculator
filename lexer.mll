{
    open Parser    
    exception Eof
}

let digit = ['0'-'9']
let space    = [' ' '\t']
let indice = '[' space* digit+ space* ',' space* digit+ space* ']'
let range = '(' space* indice space* ':' space* indice space* ')'

rule token = parse
    [' ' '\t']     { token lexbuf }
    | indice as lxm        {INDICES(lxm)}
    | range as lxm         {RANGES(lxm)}
    | ['\n' ]        {EOL}
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | ['-']?(['1'-'9']+['0'-'9']* | ['0'])['.']?(['0'-'9']*['1'-'9']+ | ['0']) as lxm  {FLOAT(float_of_string lxm)}
    | ','           {COMMA}
    | ':'           {COLON}
    | '('           {PROPEN}
    | ":="          {ASSIGN}
    | ')'           {PRCLOSE}
    | '['           {BROPEN}
    | ']'           {BRCLOSE}
    | ';'           {TERM}
    | "COUNT"       {COUNT}
    | "ROWCOUNT"    {ROWCOUNT}
    | "COLCOUNT"    {COLCOUNT}
    | "SUM"         {SUM}
    | "ROWSUM"      {ROWSUM}
    | "COLSUM"      {COLSUM}
    | "AVG"         {AVG}
    | "ROWAVG"      {ROWAVG}
    | "COLAVG"      {COLAVG}
    | "MIN"         {MIN}
    | "ROWMIN"      {ROWMIN}
    | "COLMIN"      {COLMIN}
    | "MAX"         {MAX}
    | "ROWMAX"      {ROWMAX}
    | "COLMAX"      {COLMAX}
    | "ADD"         {ADD}
    | "SUBT"        {SUBT}
    | "MULT"        {MULT}
    | "DIV"         {DIV}
    | eof           {raise Eof}
