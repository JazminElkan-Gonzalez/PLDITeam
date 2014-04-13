Ariana Chae, Jazmin Gonzalez-Rivero, Caitlin Riley

To compile:
	1) Start SML/NJ
	2) Type "CM.make "make.cm"
	3) Type "Shell.run[]"

Sample outputs:

	Example of errors caught in the parser:

		- Shell.run[];
		Type . by itself to quit
		Type :parse <expr> to see the parse of expression <expr>
		Initial environment: add sub interval hd tl cons nil map filter equal less 
		project> let double x = x + in double 10
		Parsing error: error in let - expected expr 
		project> [1,]
		Parsing error: error in map or filter - expected bar 
		error in interval - expected ddots

	Example of errors caught in the evaluator:

		- Shell.run[];
		Type . by itself to quit
		Type :parse <expr> to see the parse of expression <expr>
		Initial environment: add sub interval hd tl cons nil map filter equal less 
		project> cons 21 meow
		Evaluation error: There is no function called meow, please check your spelling or your inputs
		project> hd 20
		Evaluation error: Error: Not a list
		project> interval 1 hi
		Evaluation error: There is no function called hi, please check your spelling or your inputs
		project> #c {a=10, b=20}
		Evaluation error: There is no function called c, please check your spelling or your inputs


Project organization:

	In order to allow for the shell to catch for syntax errors, we have changed the errors in the evaluator to be more descriptive, and we are updating and returning a reference string to specify an error every time a program fails to move on to the next step in a function in the parser.


Next steps:

	Now that we have figured out how to help the user recognize where they made a syntax error in their program, the next step is to find a way to save the tokens so that they do not have to rewrite the entire program and parse/evaluate it all over again.