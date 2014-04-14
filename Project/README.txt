Ariana Chae, Jazmin Gonzalez-Rivero, Caitlin Riley

To compile:
	1) Start SML/NJ
	2) Type "CM.make "make.cm";"
	3) Type "Shell.run[];"

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

	The bulk of the work done thus far for our project has been trial and error to find the best ways to implement different aspects of our project. Although the final method we have decided on appears quite simple, we have determined that it is the best way to accomplish our goals at the moment. Some methods we have explored include creating our own SOME and NONE option datatypes, trying to have a reference pointer to pass our tokens to automate the process of creating error messages (as opposed to our current option), exploring different places to put our external files to read to (i.e. creating a primitive function in the evaluator), and more. We have also figured out how to use sockets to read/write to and from files.

	In order to allow for the shell to catch for syntax errors, we have changed the errors in the evaluator to be more descriptive, and we are updating and returning a reference string to specify an error every time a program fails to move on to the next step in a function in the parser. We also pulled code from homework 4 and lecture 10 (the lazy evaluator) to implement better function and variable definitions.


Next steps:

	The next step is to find a way to create an environment from within an environment (or something similar) to allow us to dynamically pull someone's file and check it as they type. This means we are moving the user input away from the shell, although it will be still be running in parallel, and moving towards reading programs from an external file which will then be parsed. This will take us one step closer to our goal of creating a dynamic syntax checker for our language's surface syntax.

	From here, if we have time, we will make more intelligent error messages to help the users more accurately diagnose their problems.