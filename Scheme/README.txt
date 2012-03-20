Here's a crazy straight-C implementation I did of a tiny subset of Scheme in one sleepless weekend. I know it says ".cpp", but Visual Studio doesn't let you write in pure C. I promise I only used the contents of whatever MS' version of "stdio.h" is. What makes it fun is that:

 1) I hadn't programmed C since I was ~13, and back then I didn't even know about dynamic memory allocation (that's right, no malloc). This was my first C written since becoming a big-boy programmer and knowing about heaps and so forth. 
 
 2) Over the course of the weekend, my buddies, avid game programmers, were having their own hack-a-thon at my apartment. They took immense pleasure in watching me struggle with the low-levelness after all the smug LINQ I had rubbed in their faces over the years. 
 
 3) I had never written a language implementation of any sort before, including a parser, lexer, interpreter, any of that jazz. I worked on a little corner of one at my day job, but nothing nitty-gritty. 
 
 4) I refused to look at any but the most tangentially related tutorials. Which brings us to 
 
 5) I was reading SICP and decided to just take the meta-circular evaluator chapter and write it in C. Which means I basically encoded a Lisp-style list of list of lists... as a tagged union, and coded up the interpreter using those, basically writing lisp in C. It is comically unreadable.