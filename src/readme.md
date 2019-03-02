Readme for the Abella Scripts for \
_A Mechanical Formalization of Higher-Ranked Polymorphic Type Inference_
======

Required Version
------
Abella v2.0.7-dev, commit `21ef1d` or above
in the master branch of its GitHub repository: <https://github.com/abella-prover/abella>.

How to Build Abella
------
The official description is in the *Download* section of its project webpage: <http://abella-prover.org>.

But we give another step-by-step instruction here, in case the reader is not familiar with Ocaml.

- A Linux machine is preferred due to the convenience on package management
- Install Ocaml with OPAM: <https://opam.ocaml.org/doc/Install.html>
- Install [findlib](http://projects.camlcity.org/projects/findlib.html) (manually build)
    and [ocamlbuild](https://github.com/ocaml/ocamlbuild) (via opam)
- Build and install Abella (master branch of the git repo: <https://github.com/abella-prover/abella>)
- Run the abella executable and you should see: `Welcome to Abella 2.0.7-dev.`

How to Compile/Re-check the Proofs
------
- Execute `abella` in the `src` folder in this auxiliary pack
- Enter the command `Import "decidability".` as the last file in our development which depends on every other scripts, and wait for Abella to compile all the dependencies
- Success with no errors indicates that the proofs are accepted
- Use `Quit.` to exit the interactive program.
- The shell command `grep "skip" *.thm` further confirms that no theorems/lemmas are assumed but not proven.

Explore Around the Proofs
------
- The reader is expected to be familiar with Abella or similar system with HOAS representations. Experiences on other proof assistants definitely helps, but the proofs might look different and thus difficult to understand some critical details.
- `typing.thm` is the starting point that defines all the types, terms, declarative rules and algorithmic rules.
- `trans.thm` defines worklist instantiation(`tex`) and declarative transfer (`dc`)
- `declTyping.thm` argues the equivalence of the original declarative system and the non-overlapping variant
- `soundness.thm` contains a lot of uninteresting lemmas on the shape invariance during worklist instantiation
- `dcl.thm` introduces another equivalent definition `dcl` to declarative transfer `dc`, which encodes the non-overlapping declarative worklist with a step-by-step style, used for the induction of the complteness theorem.
- `completeness.thm` also contains a lot of uninteresting lemmas as soundness do.
- `inst_decidable.thm` and `decidability.thm` proves the termination results
