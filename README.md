# C0 bytecode compiler & virtual machine

Welcome to the OCaml/C0 bytecode compiler and virtual machine.  The objective is
to learn how the OCaml bytecode compiler and interpreter works by writing a similar system for the
[C0 language](http://c0.typesafety.net), a tiny safe dialect of C.

**WARNING:** right now many things do not work and many others are missing. Having
said this, it should be possible to do simple experiments with the compiler and
see some reasonable output (see below for an example).

## Installation

Clone from git and install manually:
```sh
cd ~/tmp
git clone https://github.com/nojb/ocaml-c0
cd ocaml-c0
make
```
At this point there will be a binary `main.native` which is the bytecode compiler.

### What works?

- Variables (scalar and structured types)
- Control structures (while and for loops, break, continue)
- Functions
- Primitive command line batch "compiler" (it does not actually output any
  bytecode yet, but it can print it so that we can look at it!)

### What is missing?

- Contracts (see the C0 language reference)
- Bytecode interpreter
- Object file production
- Multi-file compilation/Linker

### Example

Write the following in `file.c0`.
```sh
int fact (int acc, int n)
{
  if (n = 0) {
    return acc;
  } else {
    return (fact (n * acc, n-1))
  }
}  
```

We can see the three phases of the compiler that are implemented so far
(parsing, elaboration & translation into bytecode) by running
```sh
./main.native -dparsetree -dlambda -dinstr file.c0
```
You should see a lot of rather readable output.  For example the output of the `-dinstr` switch
should look something like:
```sh
L1:	const 0
	push
	loadi 3
	intcomp "=="
	branchifnot L2
	loadi 1
	return 2
L2:	const 1
	push
	loadi 3
	subint
	push
	loadi 2
	push
	loadi 4
	mulint
	push
	tailcall 2, 2, L1
```

## Comments

Comments, bug reports and feature requests are very welcome: n.oje.bar@gmail.com.
