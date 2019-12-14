# Rooster-Spec

Rooster-Spec is a Coq plug-in for automated theory exploration. You give Rooster-Spec a set of (computable) functions, and it returns a list of interesting (unproven) conjectures about these functions.

Note that this plug-in is still in very early development stages! We encourage people to try it out and report any bugs.

## Installation (linux/unix only currently)

Prerequisites:

- opam >= 2.0.5
- dune >= 1.10
- coq 8.9.*
- stack >= 2.1
- Z3
- CoqHammer
- clone [this](https://github.com/mikkelmilo/tip-tools) fork of tip-tools, and follow the installation instructions

After installing all the prerequisites, in the entry folder of rooster-spec run `make install`.

## Usage

First import the plug-in and the Extraction plug-in
`Declare ML Module "coq_spec"`
`From Coq Require Import Extraction`

Then simply execute the command
`DiscoverLemmas "<somefilename>" func1 func2 func3.`

If it runs too slow, or you want larger, more complex conjectures, you can change the maximal term size (default is 5) with `MaxTermSize <size>`.

see the .v files in theories/ for examples.
