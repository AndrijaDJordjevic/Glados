# Language Syntax BNF

## Variables
***<const\>*** ::= <int\>| <float\>| <bool\> | <char\> | <string\> | <list\> | <null\>  
***<char\>*** ::= "'" <ascii-char\> "'"  
***<list\>*** ::= "[" <item-list\> "]"  
***<int\>*** ::= "" | <digit\> <int\>  
***<float\>*** ::= <int\> "." <int\>  
***<bool\>*** ::= "True" | "False"  
***<null\>*** ::= "nil"  
***<string\>*** ::= '"' <ascii-char\> '"'  

## Statements
***<syntax\>*** ::= "" | <statement\> <syntax\>  
***<item\>*** ::= <infix\> | <unary\> | <function-call\> | <const\> | <symbol\> | "(" <item\> ")"  

***<statement\>*** ::= <item\> ";" | <if-tree\> | <while-statement\> | <for-statement\> | <function-definition\>  
***<if-tree\>*** ::= <if-statement\> <elif-statement\> <else-statement\>  
***<if-statement\>*** ::= "if" <condition\> <code-block\>  
***<elif-statement\>*** ::= "" | "elif" <condition\> <code-block\>  
***<else-statement\>*** ::= "" | "else" <code-block\>  

***<while-statement\>*** ::= "while" <condition\> <code-block\>  
***<for-statemen\t\>*** ::= "for" "(" <item\> ";" <item\> ";" <item\> ")" <code-block\>  

***<function-definition\>*** ::= "func" <symbol\> "(" <symbol-list\> ")" "{" () "}"  
***<code-block\>*** = "{" <syntax\> "}"  
***<condition\>*** = <condition\>  
***<function-statement\>*** ::= "" | <syntax\> | "return" <item\> ";"  
***<function-call\>*** ::= <symbol\> "(" <item-list\> ")"  

## Operators
***<infix\>*** ::= <item\> <infix-operator\> <item\>  
***<unary\>*** ::= <unary-operator\> <item\>  


## Types
***<digit\>*** ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"  
***<letter\>*** ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"   
***<complex-char\>*** ::= "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"  
***<symbol\>*** :== <string\>  

***<ascii-char\>*** ::= <complex-char\> | <letter\> | <digit\> | "\n" | "\t" | "\r"  
***<infix-operator\>*** ::= "+" | "-" | "*" | "/" | "%" | "&&" | "||" | "==" | "=" | "<=" | "<" | ">=" | ">" | "!="  

***<unary-operator\>*** ::= "+" | "-" | "not" 

***<symbol-list\>*** ::= "" | <symbol\> <symbol-list\> 
***<item-list\>*** ::= "" | <item\> <item-list\> 
