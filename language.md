# Langauge Reference

### Tokens
* arithmetic operators: PLUS(+), MINUS(-), TIMES(*), DIVIDE(/), POWER(^)
* comparison operators: EQ(==), NEQ(!=), LT(<), GT(>), LEQ(<=), GEQ(>=)
* boolean operators: NOT(!), AND(&), OR(|)
* definition operators: ASSIGN(=), ARROW(->)
* piecewise operator: SEMI(;)
* dividers: LPAREN and RPAREN(()), LBRACE and RBRACE([]), COMMA(,)
* reserved words: DEFINE("def"), CONST("con"), N("n"), T("t")
* ID - Any combination of letters(A-Z, a-z), numerals(0-9), and underscores(_) which is not a keyword and begins with a letter
* FID - any valid id followed by a LPAREN
* FFID - any valid id followed by a LBRACE
* FLTLIT - one or more numerals with a decimal point in any order
* INTLIT - one or more numerals without a decimal point

### Context-Free Grammar
* Terminals: any token or EOF or epsilon(empty string)
* Nonterminals
  * program --> decl_list EOF
  * decl_list --> epsilon | decl_list decl
  * decl --> DEFINE func ASSIGN def | CONST ID ASSIGN def
  * func --> ID | ID LPAREN formals_opt RPAREN | ID LBRACE formal_funcs RBRACE LPAREN formals_opt RPAREN
  * def --> decl_list ARROW expr | expr
  * expr --> expr SEMI expr | expr OR expr | expr AND expr | expr LT expr 
  | expr LEQ expr | expr GT expr | expr GEQ expr | expr EQ expr | expr NEQ expr 
  | expr PLUS term | expr MINUS term
  * term --> term TIMES factor | term noneg_factor | term DIVIDE factor | factor
  * factor --> value POWER factor | MINUS factor | NOT factor | value
  * noneg_factor --> value POWER factor | NOT factor | value
  * value --> INTLIT | FLTLIT | N | T | call | LPAREN expr RPAREN
  * call --> ID | FID actuals_opt RPAREN | FFID id_list RBRACE LPAREN actuals_opt RPAREN
  * formals_opt --> epsilon | id_list
  * id_list --> ID | id_list COMMA ID
  * actuals_opt --> epsilon | actuals_list
  * actuals_list --> expr | actuals_list COMMA expr

### Operator Precedence
1. ^ (right assoc.)
2. ! unary_minus("-") (right assoc.)
3. \* / (left assoc.)
4. \+ \- (left assoc.)
5. < > <= >= (left assoc.) 
6. == != (left assoc.)
7. & (left assoc.)
8. | (left assoc.)
9. ; (left assoc.) 

### Comments
* multi-line: any text after a leading "{\*" and before a trailing "\*}"

### Data Types
* float: IEEE 754 floating point, used in arithmetic. 
Also used for boolean algebra - 0 equals FALSE and any other value equals TRUE 
* int: 32-bit, only used for function argument signatures

### Operators
* arithmetic
  * \+ : floating point addition
  * \- : floating point subtraction or sign negation of RHS
  * \* : floating point multiplication
  * / : floating point division (note: division by 0 is undefined)
  * ^ : floating point exponentiation 
  (note: a negative base and factional exponent is undefined, as is 0 raised to 0)
* comparison
  * == : evaluates to 1 if LHS is equal in value to RHS. Otherwise evaluates to 0
  * != : evaluates to 1 if LHS is not equal in value to RHS. Otherwise evaluates to 0
  * < : evaluates to 1 if LHS is less than RHS. Otherwise evaluates to 0
  * \> : evaluates to 1 if LHS is greater than RHS. Otherwise evaluates to 0
  * <= : evaluates to 1 if LHS is less than or equal to RHS. Otherwise evaluates to 0
  * \>= : evaluates to 1 if LHS is greater than or equal to RHS. Otherwise evaluates to 0
* boolean
  * & : evaluates to 1 if LHS and RHS are nonzero in value. Otherwise evaluates to 0
  * | : evaluates to 1 if LHS or RHS are nonzero in value. Otherwise evaluates to 0
  * ! : evaluates to 1 if RHS equals 0. Otherwise evaluates to 0
* piecewise operator
  * ; : evaluates to the LHS if it is defined. Otherwise evaluates to the RHS

### Declarations
1. Function - Consists of the "def" keyword, a function signature, the "=" sign, and a definition. Defines a new callable function.
    * function signatures consist of 3 parts:
      1. name - the id which is used to call the function. Can be any valid id
      2. value arguments - determines which values are passed into the function when called and their names inside its definition. Is a list of comma-separated ids enclosed by parentheses. The parentheses are optional if there are no value arguments or function arguments
      3. function arguments - determines which functions are passed into the function when called and their names inside its definition. Is a list of ids with each id followed by two integers between the "<" and ">" symbols specifying how many value and function arguments the function argument should take
2. Constant - Consists of the "con" keyword, an id, the "=" sign, and a definition

### Definitions
* Always includes an expression which determines what the function or constant returns when called 
* Optionally, the definition can also contain a list of nested declarations. The outer function is said to "enclose" these nested definitions
* The nested declarations are only visible within the definition of the enclosing function. They are said to be in the "scope" of that definition
* the "->" must preceed the expression if the definition includes a non-empty list of nested declarations

### Inputs and Outputs
* "n" and "t" are input variables which can be placed in expressions at any location in the program.
* "x" and "y" are special functions which determine the output graph by defining x and y coordintes of each point parametrically in terms of "n" and "t"
* "nstart", "nstop", "nstep", "tstart", "tstop", and "tstep" are special constants to define the range of "n" and "t". 
Each input can be given a start value, a stop value, and a step value. 
The range begins at the start value and increments by its step until it is greater than the stop value. 
By default, the values if not declared explicitly are as follows: 
  * nstart = -10 
  * nstop = nstart + 20 
  * nstep = 0.1 
  * tstart = 0 
  * tstop = tstop + 10
  * tstep = 0.1

### Evaluation Rules
* Constants are evaluated first, from top to bottom and left to right. Constant expressions are just like function expressions except they cannot depend on "n" or "t"
* After, x and y are evaluated for each value of "n" in "t" in their ranges

### Visibility 
* a function is callable in any code that comes after the "=" symbol of its declaration. Thus, it is callable by any declaration after it in the same scope, any declaration it encloses, and recursively from within its own definition
* a constant is callable by any code in the scope in which it was declared, any scopes enclosed by that scope, any scope it encloses, and recursively (note: this means globally scoped constants can be called anywhere) 
* a value argument or function argument is callable from any enclosing scope of the function to which it belongs

### Coverage
* if two callable functions have the same name, then the later declaration covers the earlier declaration
* if two callable function arguments share the same name, then the argument in the tighter enclosing scope covers the other. If they are in the same scope, then the second one listed covers the first
* if a function argument shares a name with a declaration, then the one of them in the tightest enclosing scope covers the other. 
If they are in the same scope (ie: the argument to the function and a nested declaration), then the function declaration covers the function argument
* if two constants have the same name and are both visible, then the call returns the value of the last time the constant was evaluated
* if two value arguments have the same name and are both visible, then the call returns the value of whichever one is in the tighter enclosing scope. If the scope is the same, it returns the value of the one listed later
* if a value argument and a constant declaration are both visible, then the call returns the value of whichever one has the tighter enclosing scope. If they have the same scope, then the value of the constant is returned 
* if any visible of (function, function argument) have the same name as any visible of (constant, value argument), then the coverage depends on if the call has parentheses or braces directly after the id. If these are included, the call corresponds to the function or function argument. 
Otherwise, if it is just the name, then the call is presumed to refer to the constant or value argument, even if the visible function or function argument has no arguments







