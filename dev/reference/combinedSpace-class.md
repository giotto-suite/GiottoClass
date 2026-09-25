# Class for a frame shared by named samples

A frame that several samples are laid out in together — the cross-sample
case. Its membership is the whole difference from a
[perSampleSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/perSampleSpace-class.md):
it says these samples occupy ONE coordinate system, so a job built here
is one job spanning them rather than one per sample (`adr/0006`).

Membership is DERIVED from the steps — every sample any step names, in
recorded order, readable with
[`names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/names.md).
It is not a slot, so there is nothing that can disagree with the recipe.
A member that needs no transform of its own is declared with a
membership step, which
[`combinedSpace()`](https://giotto-suite.github.io/GiottoClass/dev/reference/space-constructors.md)
seeds and `samples =` on any transform verb adds to.

**Declaration-only.** `space = "<name>"` on a transform verb creates a
[perSampleSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/perSampleSpace-class.md);
saying that samples share a coordinate system is a real claim and is
made out loud, with
[`combinedSpace()`](https://giotto-suite.github.io/GiottoClass/dev/reference/space-constructors.md).
It also writes its artifact at the parent, where there is deliberately
nowhere to put per-sample content back, so the round trip that a
per-sample job closes does not close here.

## Value

a `combinedSpace` object

## See also

[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace-class.md),
[`combinedSpace()`](https://giotto-suite.github.io/GiottoClass/dev/reference/space-constructors.md)
