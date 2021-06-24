OCaml source code for the following paper presented at FSCD 2021:  

Maika Fujii and Kenichi Asai
"Derivation of a Virtual Machine for
 Four Variants of Delimited-Control Operators"
6th International Conference on Formal Structures for Computation and
Deduction, FSCD 2021, July 17-24, 2021, Buenos Aires, Argentina
(Virtual Conference), volume 195 of LIPIcs, pages 16:1-16:19,
doi:10.4230/LIPIcs.FSCD.2021.16.  

To compile, you need OCamlMakefile.  Write the location of the
OCamlMakefile in eval*/Makefile and make.  To execute the interpreter,
run ./interpreter and input the program into the standard input.
Example inputs:


``` 1 + reset (reset (2 * reset ((fun y -> shift h -> y)
    	  	      	     (shift f -> shift g -> 3 + f 4))))

1 + reset (reset (2 * reset ((fun y -> control h -> y)
    	  	      	     (control f -> control g -> 3 + f 4))))

1 + reset (reset (2 * reset ((fun y -> shift0 h -> y)
    	  	      	     (shift0 f -> shift0 g -> 3 + f 4))))

1 + reset (reset (2 * reset ((fun y -> control0 h -> y)
    	  	      	     (control0 f -> control0 g -> 3 + f 4))) ```   


eval1/		Listing 1: definitional interpreter  
eval2/		Listing 2: defunctionalized interpreter  
eval3/		Listing 3: linearized interpreter  
eval4/		Listing 4: stack-based interpreter  
eval5/		Listing 5: delinearized interpreter  
eval6/		Listing 6: refunctionalized interpreter  
eval7/		Listing 7: interpreter with combined arguments  
eval8/		Listing 8: interpreter with functional instructions  
eval9/		Listing 9: interpreter with defunctionalized instructions  
eval10/		(Listing 10): interpreter with linearized instructions  
eval11/		Listing 11: interpreter with linearized trails  
