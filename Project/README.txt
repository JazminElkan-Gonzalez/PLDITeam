Project Description:

Our team created a dynamic syntax checker for a very basic functional programming language written in SML. We built off of a language we worked with in a previous homework (homework 4) combined with code from lecture 10 and found that the error messages we were getting from the language did not provide us with sufficient information to diagnose syntactical errors that we made. In order to implement a smart syntax checker, we had to change the lexer and parser to be able to handle incomplete programs and programs with newlines, and then take the data and present it in a useful way to help the user identify and correct their mistake. We did this by catching when an error occurred and then presenting the tokens parsed thus far and the tokens left over, showing the user where they missed an expected token.

We ran into some issues with nested functions and with cases where a program could expect multiple types of tokens, and we also had to find a way to store and update the information gathered so as not to lose any useful markers as to where the syntax error occurred. As a result, there are times when a program fails and there are many errors shown. This usually occurs when a given expression type (such as “let”) is used more than twice in the same line. For this project we also wanted to focus on there only being one error at a time. We find that usually at the very least one of the error messages displays a comprehensible error message that can help the user deduce their mistake.

The IDE portion of the dynamic syntax checker is a simple Windows Form written in C#. After the user stops typing for a specific length of time, the program saves what the user has written so far in a plain text file and starts an sml process that tokenizes, parses, and evaluates the contents of that file. The standard output of this process is redirected so that the result of parsing and evaluation can be displayed in the IDE. In the event of a parsing or evaluation error, the first line that generated an error is underlined. Currently the file paths used to start sml and save and check the file are relative to one computer, so it cannot actually be run successfully on another computer without changing those file paths. The code for the IDE can be found in the SimpleIDE folder.

To compile and run our language from the command line:
	1) Start SML/NJ in the folder containing make.cm
	2) Type "CM.make "make.cm";"
	3) Type "Shell.run [];"


Sample executions:

(These examples were run through the terminal; however, the outputs would be the same using the more dynamic Windows form.)

project> let double x = x + in double 10  
Parsing error: error in let- expected expr
let double x = <expr> in double 10  
x + <term> 

project> let max a = if (a<b) then b else a in max 10 20
Evaluation error: There is no function called b, please check your spelling or your inputs

project> 1::(2::)
Parsing error: error in Append List- expected C term
1 :: <term> 
( <expr> )  
2 :: <term> 

project> match (1::) with [] -> 1 | x::xs -> 2
Parsing error: error in match- expected expr
match <expr> with [ ] -> 1 | x :: xs -> 2  
( <expr> ) 
1 :: <term>

project> match (1::nil) with [] -> 1 x::xs -> 2
Parsing error: error in match- expected bar
match ( 1 :: nil ) with [ ] -> 1 x :: xs '|'-> 2 

project> [3,2,4,]
Parsing error: error in list- expected expr
[ 3 , 2 , 4 , 'expr'] 

project> interval (1+2) (3+4 
Parsing error: error in parentheses- expected right parentheses
( 3 + 4 ')'Function Call Failed 

project> [1..]
Parsing error: error in interval- expected expr
[ 1 .. <expr> ] 

project> [(1+2)..(4+)]
Parsing error: error in interval- expected expr
[ ( <expr> ) .. ( 4 + ) ]  
1 + <term> 
2 ) .. <expr> ]... ) 
( <expr> ) 
4 + <term> 

project> [x|x <- [1,2,,4]]
Parsing error: error in list- expected expr
[ x | x <- [ 1 , 2 , 'expr', 4 ] ] 

project> [add 10 x | x [1,2,3,4]]
Parsing error: error in map or filter- expected left arrow
[ add 10 x | x '<-'[ 1 , 2 , 3 , 4 ] ] 

project> filter (\x true) [1,2,3,4]
Parsing error: error in function- expected right arrow
( / x '->'true ) [ 1 , 2 , 3 , 4 ] Function Call Failed

project> [x|x <- [1,2,3,4], tru]
Evaluation error: There is no function called tru, please check your spelling or your inputs


project> [x + x | x <- [1,2,3,4,5,6,7], x < 3
Parsing error: error in filter- expected right bracket
[ x + x | x <- [ 1 , 2 , 3 , 4 , 5 , 6 , 7 ] , x > 3 ']'

project> #a {a = 10, b = }
Parsing error: error in field- expected expr
# a <expr> 
{ <field> } 

project> {a=20; b=20}
Parsing error: cannot tokenize ; b=20}

Things to note:

- We implemented a type checking function (evaluator.sml, line 16) for our primitive types to use for debugging, because there is no easy way in SML to check the type of an object.
- Throughout evaluator.sml, we made several smaller changes to accommodate for changes we made in how lists are created and changed and how functions are created.
- Some of our functions were syntactically similar, making it hard to tell which function the user was trying to call when they made an error. (For example, the syntax for map, filter, interval and list all encompass brackets and expressions and are hard to differentiate.) We combined the functions into one giant function (parser.sml, line 697) to accommodate this issue and improve our error messages.
