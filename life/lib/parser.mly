%{
    open Rule
%}

%token <int> INT
%token S
%token B
%token SLASH
%token EOF

%start <Rule.rule> prog

%%

prog:
    | srule = S_rule; SLASH; brule = B_rule EOF { { survive = srule; birth = brule } }
;

S_rule:
    | S; nums = num_list { nums }
;

B_rule:
    | B; nums = num_list { nums }
;

num_list:
    | { [] }
    | digit = INT; rest = num_list { digit :: rest}
;
