# gAny virtual class

Base extension point for giotto-compatible analysis objects. Acts purely
as a dispatch tag, so shared-domain methods (expression, dim reduction,
NN networks, cell metadata, etc.) are written once via
`setMethod("foo", "gAny", ...)` and apply to every container that opts
in.

Defines no slots. A subclass is free to represent shared concepts
however it likes, and `gAny`-level methods reach them through accessor
generics rather than direct slot access. That is what lets a container
participate in the shared-domain API without inheriting `giotto`'s
spatial slots — and without spatial-domain methods silently falling
through to slots it does not have. An unimplemented method fails loudly
via no-method dispatch instead.

Exported so that analysis packages can attach their container-level
methods to `gAny` too, covering `giotto` and `giottoMulti` with one
method.
