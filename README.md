# maybe_compiler
trying to implement compiler in Lazarus according to 'dragon book' 

## Current status of project
- prints parse tree of the 'program' provided in text window or from file
- prints syntax tree of the 'program' provided in text window or from file
- prints postfix translation of expressions provided in text window or from file
- error messages, syntax tree and postfix translation  are currently output to console only

## How to run it
Open project file 'compiler.lpi' in Lazarus and run it

## Syntax
Grammar is described in head comment of 'parser.pas'. 
Here is summary:
- program must be enclosed in curly brackets { }
- block inside program must be enclosed in curly brackets
- statements must be finished with ';'
- indentation does not matter
- variables used inside program or block must be declared before use. Possible types 'num' and 'bool'.
- numerical expressions can use operators =. +. -. *, / and parens ( )
- boolean expressions can use operators ||, && and parens (). For values 
- integer and decimal numbers are accepted
- bool values are case sensitive 'true' and 'false' 

### Program example
		{
        num x;
        num y;
        bool z;
        	{
            x = 34/5 - (2*8);
            }
        y = 3 * x;
        z = true || (false && true);
    }
