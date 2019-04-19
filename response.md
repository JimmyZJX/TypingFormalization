
Response for Submission 66, ICFP 2019
======

We are grateful for the helpful reviews and are happy
that you appreciate our work. In what follows we
respond to your main comments.

Review A
------

> The difference between the presentation in the paper and the Abella mechanization.

In the paper, the main algorithmic rules are written in a simple small
step style with no premise on the reduction relation (-->) itself.
And the rules are not overlapping with each other.  These properties
indicates that a trivial translation to the big step style used in the
proofs preserves equivalence.

We believe the presentation of the algorithmic rules in the paper is
clearer and restricted to the form $\Gamma$ --> $\Gamma$, while
the big step unary relation in the proof simplifies the induction
scheme.  More importantly, the difference is just a matter of
presentation, but the logic behind remains unchanged.
We will mention this difference in presentation clearly in the paper.

> The restriction of subtyping to simple  types is effectively an
> equivalence (and thus symmetric).  Is this correct?

Yes, this is correct. Note also that DK's algorithm also suggests a
similar symmetric unification.

> The proof scripts are hard to read, and some parts are inconsistent with the paper.

We apologize for the state of the proof scripts. Currently, we are refactoring them,
and will document the mapping between the paper and the mechanisation
in the final version of the paper.

> How does polynomial complexity follow from the reduction rule being deterministic?

We did not justify the point clearly in the paper.
We will either provide a detailed proof in the final version, or omit
this informal claim.

> Why not treat the languages of declarative and algorithmic worklists as subsets?

The proof script allows one to treat them as subsets.
However, we explicitly treat them as different syntactic sorts in the paper
to avoid potential confusion.

> I could not find a direct counterpart of this theorem in the Abella script.

The `decidable_thm` Theorem is the one that proves the Termination Theorem (4.12),
following what is described from L918, and the `decidable` Theorem is its pretty version.
We will make the theorem names consistent in the final version.

Review B
------

> General hints/guidlines about the formalization of the properties of
> an inference algorithm for a complex typing descipline.

That is a good point. We believe that our approach can indeed be
adapted to other bidirectional type systems, and we will extend the
discussion in the paper to point this out.  We are also happy to see
that Reviewer C already looks forward to trying out our approach in
other settings.
