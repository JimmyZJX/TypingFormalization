
Response for Submission 66, ICFP 2019
======

We thank the reviewers for their helpful comments and positive reception of the paper.
We answer main questions below.

Review A
------
<!--
One minor point of criticism I have is that there appear to be some unexplained differences between the presentation in the paper and the Abella mechanization, which made it harder than necessary for me to relate the two and to follow some of the proof sketches in the paper. For example, the (binary) multi-step reduction relations on algorithmic and declarative worklists seem to be represented by unary predicates (judge and dc respectively) in the Abella script, which look more like big-step than small-step reduction relations. This change of representation may even impact the structure of some proofs (e.g. the induction hypothesis used in the proofs of soundness and completeness). I know very little about Abella, so this choice of representation may well be more idiomatic than the one presented in the paper. Still, I would have liked to see a short discussion of the pros/cons of the two representations and why different representations are used in the paper and the proof script. More extensive documentation of the proof script (e.g. more comments) would also be helpful in navigating the mechanization.
-->

> The difference between the presentation in the paper and the Abella mechanization.

In the paper, the main algorithmic rules are written in a simple small step style
with no premise on the reduction relation (-->) itself.
And the rules are not overlapping with each other.
These properties indicates that a trivial translation to
the big step style used in the proofs preserves equivalence.

We believe the presentation of the algorithmic rules in the paper is much clear
and restricted to the form $\Gamma$ --> $\Gamma$,
while the big step unary relation in the proof simplfies the induction scheme.
More importantly, the difference is just a matter of presentation,
but the logic behind remains unchanged.

> The proof scripts are hard to read, and some parts are inconsistent with the paper.

We apologize for the lack of clean-up on the proof scripts.
We are now refactoring the scripts,
after that we will provide a better documentation on the encoding details
that might cover all the concerns by the reviewer,
preferrably to the appendix of the paper.


Review B
------
<!--
On the negative part, I am not sure that one can, from the paper, infer general hints/guidelines about the formalization of the correctness/completeness/... of an inference algorithm for a complex typing discipline. This is mainly due to the fact that the authors concentrate their attention to one single inference algorithm.
-->
<!--
Tom:
- Apparently Reviewer C believes they will be able to try out our approach on new problems.
- I also think there is an obvious generalization to other bidirectional type systems.
- We can say that we will extend the discussion to point this out.
-->

> General hints/guidlines about the formalization of the properties of an inference algorithm for a complex typing descipline.

We believe that the algorithm proposed can adapt to other bidirectional type systems,
and we will further extend the discussion to point this out.


Review C
------
<!--
In the course of their investigation, the authors found several problems with the mathematical proof DK give and provided alternatives. I am not in a position to comment on whether these criticisms of DK are justified, nor do I find this significant for the evaluation of the paper.
-->
<!--
Jimmy:
If the reviewer are not interested in the criticisms,
we can reduce relevant discussion in the paper.
My point was that I am not sure whether DK's algorithm and proofs
could be easily formalized, but it seems no easy.
And I decided not to take time investigate,
given that there are already some issues, though they might be minor.
-->

> Criticisms of DK's proof

...

