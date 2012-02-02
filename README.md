Erasm++ 0.1.2
=============

What is this ?
--------------

Erasm++ is a collection of C++ libraries for high-performance runtime
code generation, instruction decoding, and metaprogramming. It
consists of the following three components:

* ERASM++, a library for runtime code generation on x86/x64
  architectures,

* GenericDsm, a fast generic instruction decoder library, and

* MetaPrelude, a library for Haskell-like lazy (but untyped)
  metaprogramming.

Documentation
-------------

A preliminary [online manual](http://nishiuramakoto.github.com/erasm-plusplus/) was
constructed.  Alternatively, a PDF manual was also prepared. Please see
doc/manual.pdf for an off-line documentation.

Installation
------------
Please see INSTALL for system requirements and installation instructions.


Brief overview of the libraries
-------------------------------
Here I describe each component briefly.


### ERASM++ ###

ERASM++, the Embedded Runtime Assembler in C++, is an Embedded Domain
Specific Language (EDSL) in C++ for runtime code generation on Intel
64/IA-32 architectures. The code looks like the following:

    p += adc  (p,word_ptr  [edx + esi * _2],ax);
	p += and_ (p,dword_ptr [rip + 0x1000],r12d);
	p += rep_movs (p,dword_ptr.es [di],dword_ptr.gs [si]);
	// Unfortunately, lables are not yet implemented!
	p += jmp  (p, 0x10);
	
Although similar runtime code generator libraries already exist, I
believe ERASM++ has the following strengths:

* It is easier to use because the syntax is more natural.

* It is safer to use because the syntactically invalid expressions
  cause compilation errors, rather than runtime errors or exceptions.

* It is faster because most of the encoding and the error checking are
  done at compile-time, rather than at runtime.

It's so fast that the generator functions are actually compiled into a
series of MOV instructions with no external references. Please see the
documentation for the detail.

### GenericDsm ###

GenericDsm is a fast generic instruction decoder for x86/x64
architectures which allows "pattern matching" against the decoded
instructions.

The following code illustrates the idea:

	struct MyCounter : public InstructionCounter
	{
		InstructionCounter(const_code_ptr start) 
		: InstructionCounter(start)
		{}

	    // Let the base class handle the default cases 
		using InstructionCounter::action;

	    action_result_type
	    action(const Ret& insn)
        {
		  cout << Ret::mnemonic << endl;
		  return finish(insn);
        }

	    template<class Op>
	    action_result_type
	    action(const Jmp& insn,const Op& op)
        {
		  cout << Jmp::mnemonic << " " << op << endl;
		  return finish(insn);
        }
	};


Just as ERASM++, The main strength of GenericDsm is its performance.
Please see the documentation for the detail.


### MetaPrelude ###

MetaPrelude is a library for (untyped) lazy metaprogramming in C++
which tries to mimic Haskell as much as possible. It is used as the
backend of static type-level computation in ERASM++ and GenericDsm.  I
hope it should help implementing more complex EDSLs than the assembly
language.

It is still far from complete, and the important design decisions are
still open. Maybe I will write a translator from the ghc Core? But
then how can I ensure easy integration with the user defined types in
C++? I'm not sure.

Here is a list of important limitations:

* It is an untyped calculus.

* It doesn't support lambdas (efficiently).  You are expected to
  lambda-lift every unnamed function.  This means that you can't have
  Haskell-style monads syntactically.  You still have monads in
  mathematical sense, of course, as monads are just an abstraction of
  *actions* of monoids.

* In order to support currying, lots of ugly preprocessor hacks are
  involved.  Furthermore, functions with only up to a fixed number
  (only 3 at this moment) of parameters can be curried.
  
* Most importantly, the complexity of the program is dependent upon
  the particular compiler implementation! Some compilers cache
  template instantiations, some do not, some in-between, and all this
  can make a difference of linear to super-exponential complexity.

For the moment, code is the only documentation. Please see the
following files if you are interested:

* src/erasm/meta_prelude.hpp
* src/erasm/meta_leftist_heap.hpp
* src/erasm/meta_polynomial.hpp
* src/erasm/x86_addr16.hpp
* src/erasm/x86_addr32.hpp
* src/erasm/x64_addr64.hpp

License
-------
It is licensed under GPLv3.
See the file COPYING.

Contact
-------
You can write anything about the library at the
[wiki](https://github.com/nishiuramakoto/erasm-plusplus/wiki).
Or you can just contact [me](nishiuramakoto@gmail.com). Thanks!
