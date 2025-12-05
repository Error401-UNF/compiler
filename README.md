# Description
This is a "simple" (dont look inside the box to hard) compiler that compiles the following grammer into 3 address code. This could theoretically be ran with llvm i think but my compilers class didnt reqiure me to get it that far.

Additionally i made this entire thing without the help of any libraries. I found them clunky when starting this project and abandoned them in favor of manipulating the data directly. This.. was a bad choice when it came to how much time i would need to spend to make it work but it is something i am very proud of.


Here is the grammer i used as the basis for this mess:
```
program -> 
	{ block }

block ->
	stmt block
	epsilon

stmt-> 
	{ block }
	decl
	expr
	break ;
	while ( condition ) stmt
	do stmt while ( condition ) ;
	if ( condition ) stmt elif

decl -> // makes new ids
	basic arr var ;

arr - >
	\[ condition ] arr
	epsilon 

var -> // l2
	id arr

expr ->
	var = equation ;

equation ->
	object heq
	( equation ) heq
	- equation
	! condition

heq->
	+ equation
	- equation
	\* equation
	/ equation
	hcon
	epsilon

object ->
	var
	num
	real
	true
	false

condition
	object hcon
	( condition ) hcon
	! condition
	- equation

hcon -> 
	== condition
	!= condition
	 < condition
	 <= condition
	 > condition
	>= condition
	&& condition
	|| condition
	heq
	epsilon

elif ->
	else stmt
	epsilon
```

if you just want to see examples i have plenty of very specific examples in the ./Tests folder