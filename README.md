# CSCI4230-ToyDES
## Overview
I wrote this assignment in Haskell using the fixed-vector library from Hackage.  All of the code that is directly relevant to the assignment is in the ToyDES.hs file.  BitVector.hs contains my implementation of bitfields.  For the time being it's only sparsely documented as well as highly inefficient, but it should be well encapsulated enough that fixing those issues doesn't effect the homework specific code.  Chat.hs contains some code that I was playing around with to familiarize myself with networking in Haskell.

## Math
Implementing Toy DES was relatively straightforward aside from a few minor differences in things like indexing.  I used 0-based indexing for bit-indices, whereas the class notes used 1-based indexing.  This is clearly wrong<sup>[citation needed]</sup>.  I also had to change the inverse initial permutation (`initialPerm`), my `feistel` function always swaps the `(l,r)` tuple, whereas on the slides the tuple swaps in the first round but not in the second.  Rather than modifying `feistel` I hardcoded a second (un)swap into the permutation

## BitVector
I used the fixed-vector package and the DataKinds language extension to implement statically sized bitfields (found in BitVector.hs).  This gave me some extra compile time safety regarding the bit math.  The way it works is the `BitVector` type constructor takes a type level Peano integer (`Nat` defined in `GHC.TypeLits`).  When performing numeric operations (`+`,`*`,`xor`) on `BitVector`s the type system will then check that both sides of the operation have the same length.  When performing operations like `concat` or `split` the system will check that the results are the correct type.  You will get an error for instance in `iterate (lShift *** lShift) . BitVector.split . p10` if `lShift` takes a `BitVector 4` rather than a `BitVector 5`.

There are a couple of problems with my current implementation though.  The first is that the permutations take `[Int]` (a list of ints), which doesn't allow for any of the compile time safety checks.  The second problem is that my current BitVector implementation uses unboxed arrays of booleans rather than a packed bit representation like the C++ `std::vector<bool>` template specialization.  This is ridiculously inefficient, but fixing it should mostly be a matter of modifying the `BitVector` type, the homework specific code in ToyDES would not have to be changed to accommodate this.  Another problem is that Haskell's type inference system can't always disambiguate intermediate results, such as in uses of `backPermute` followed by a polymorphic function.  I resolved this by using the TypeApplications language extension (those are the `@4`, `@10`, etc...) to explicitly specify the intermediate types, but this hurts readability somewhat.  `backPermute` also has another pitfall: the compiler cannot catch out-of-bound indices, I'm not sure how to improve on this but all of the bugs I encountered were related to this issue.

## Compiling and Execution
The program can be compiled using `stack build` (entered through the commandline, download [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/)).  Stack will automatically download the dependencies (currently fixed-vector and lens, sorry they're so huge).  I've implemented a file encrypter/decrypter, and started on a chat program that is currently incomplete.  The file encrypter/decrypter can be run with:
```stack exec -- ToyDES-exe [mode] [key] [inputFile] [outputFile]```

`mode`: either `e` or `d`, which selects either encryption or decryption, respectively.  I'm not error checking this out of laziness.

`key`: an integer 0-1023

`inputFile` and `outputFile` are self explanatory I think.

The chat program doesn't work right now, I was mostly just playing around with it to get some practice with networking/sockets in Haskell.  You could run it with `stack exec Chat-exe`, but you don't want to do that.