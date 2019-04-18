
Response for Submission 66, ICFP 2019
======

We are grateful for the helpful reviews and are elated that you appreciate our work.
In what follows  we respond to your main comments.

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

We believe the presentation of the algorithmic rules in the paper is much clearer
and restricted to the form $\Gamma$ --> $\Gamma$,
while the big step unary relation in the proof simplfies the induction scheme.
More importantly, the difference is just a matter of presentation,
but the logic behind remains unchanged.

> The proof scripts are hard to read, and some parts are inconsistent with the paper.

We apologize for the state of the proof scripts. Currently, we are refactoring them.
and will document the mapping between the paper and the mechanisation.


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

That is a good point. We believe that our approach can indeed be adapted to other bidirectional type systems, and we will extend the discussion in the paper to point this out.
We are also happy to see that Reviewer C already looks forward to trying out our approach in other settings.
