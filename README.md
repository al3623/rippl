# rippl beta 6.9.420

Recursively Inferred Pure functional Programming Language

Da Hua Chen (Riddler)

Hollis Lehv (Gallblader)

Amanda Liu (Language Yoda)

Hans Montero (Prime Minister)

**Requirements**

- ocaml 4.01.0
- opam 2.0.3
- llvm 8.0.0 (installed with opam and on machine using brew/apt/..)

**Features**
1. Hello World!

   Here is how to make your own hello world program, written in Rippl :^)
   
   First, make a file called `hello.rpl` and write the following code:
   ```
   main = fun _ -> "Your cool string here!"
   ```
   Then, build the Rippl compiler in `/src` by simply running `make`.
   Now run your first ever Rippl program!!!
   ```
   cat some/path/hello.rpl | ./compiler.native 
   ```
2. A bunch of other stuff

   Coming soon...






