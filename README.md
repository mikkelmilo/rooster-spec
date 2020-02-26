# Rooster-Spec
Discover potential laws about your coq code!

Rooster-Spec is a Coq plug-in for automated theory exploration, based on Quick*Spec*. You give Rooster-Spec a set of (computable) functions, and it returns a list of (but unproven) equational conjectures about these functions.

Note that this plug-in is still in very early development stages! We encourage people to try it out and report any bugs.

## Installation (linux/unix only currently)

Prerequisites:

- opam >= 2.0.5
- dune >= 1.11
- coq 8.9.* (only tested on 8.9.1)
- stack >= 2.1.3
- Z3
- CoqHammer
- clone [this](https://github.com/mikkelmilo/tip-tools) fork of tip-tools, and follow the installation instructions

After installing all the prerequisites, in the entry folder of rooster-spec run `make install`. If you need to reinstall the plug-in, make sure to first run `make clean`.

## Usage

In a Coq file, first import the plug-in and the Extraction plug-in
`Declare ML Module "coq_spec".`

`From Coq Require Import Extraction.`

Then simply execute the command

`DiscoverLemmas "extractionFileName" func1 func2 ... funcn.`

The `extractionFileName` doesn't really matter, but can be useful for debugging if you wish to inspect the extracted Haskell code. The supplied functions can be of any arity. The only potential restriction is that they cannot in general involve the `Prop` type (although it may work if the property implements the Decidable typeclass). This is a fundamental limitation of the testing-based approach, since it requires decidable equality on all the given types.  

If it runs too slow, or you want larger, more complex conjectures, you can change the maximal term size (default is 5) with `MaxTermSize <size>`. I recommend not using sizes larger than 7 other than for testing purposes.

see the .v files in the theories folder for examples.
