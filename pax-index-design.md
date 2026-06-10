PAX Indexing Design
===================

Currently PAX does not do indexing. SB-MANUAL has separate function,
variable, datatype, and declaration indices. In the rewrite, I removed
the concept index. Let's design indexing for PAX, then maybe implement
[a subset of] it in SB-MANUAL for Texinfo as well. We'll see.

Indexing Definitions
--------------------

Indexing definitions in PAX is easy. Since `page-definitions` already
has the necessary information, we can simply list the definitions at
the end of the page as a bulleted list or something similar. The
indices can be split or grouped by definition type (e.g. "Functions
and Macros", "Types") as the user specifies.

The intermediate Markdown on the way to PDF output:

    - foo, function: __17__

where the definition's page, 17, is in bold.

Cross-referencing is also easy: PAX knows which definitions are linked
to and where.

    - foo, function: __17__, 53, 54, 68, 79

(Here, the additional page numbers are just references.)

For HTML, the intermediate Markdown could be

    - foo, function: __section-where-foo-is-defined__, [1], [2], ...

where the section ids are links, only the defining section is listed
with an id, and the cross-references are simply numbered to reduce
clutter.

We list different definitions of `foo` in the usual multi-level way:

    - foo
        - function: 17
        - compiler-macro: 99

Indexing Concepts
-----------------

If we define "concept" as a section (or a glossary term), then they
fit naturally into this scheme. We extend `defsection`:

    (defsection @defining-macros (:title "Defining Macros"
                                  :concepts (("macros" "defining")
                                             ("defining" "macros"))
      ...)

    (defsection @using-macros (:title "Using Macros"
                               :concepts (("macros" "using")
                                          ("using" "macros"))
      ...)

The Markdown for the index would have this structure:

    - defining, macros: ...
    - macros
        - defining: ...
        - using: ...
    - using, macros: ...

Usage in Docstrings
-------------------

    @DEFINING-MACROS is easy.

This renders as

    [Defining Macros][A1F9] is easy.

but it also records the `:concepts` of `@defining-macros` when the
link is generated.

If the title does not fit nicely into the text, use

    [Macros can be defined easily][@defining-macros]

which renders as

    [Macros can be defined easily][A1F9]

One could even have totally silent indexing with

    Defining macros is easy. [][@defining-macros]

Alternatively, if its :TITLE is NIL:

    Defining macros is easy. @DEFINING-MACROS

Note that all uses of @DEFINING-MACROS indexes consistently (using its
:CONCEPTS) unlike in Texinfo where you have to write

    @cindex defining @subentry macros
    @cindex macros @subentry defining

everywhere.

The Concept Namespace
---------------------

Concepts are denoted by strings (or lists thereof) and live
(conceptually) in a single, flat namespace, independent of packages.
The examples showed how they are matched. This design decision means
that independently developed libraries can use the same string to
denote different concepts by accident. Since indices are generated at
the end of `pax::@pages`, not putting multiple libraries on the same
page gets around this problem.

Even with libraries on different pages, some cross-library concept
indexing works: if a docstring in library B has
`"LIB-A::@DEFINING-MACROS"` linked in it, then the index of library A
will list the parent definition of that docstring as a referee to the
concepts listed in `@defining-macros`.

> _Note_: Many other designs were considered, several of which had
> symbols denote concepts to leverage the namespace control provided
> by the package system. Others had a way of declaring the equivalence
> of concepts in different packages along the lines of `owl:sameAs`.
> All of these designs collapsed under their own weight, as they tried
> to map ill-defined and potentially volatiles ontologies.

Implementation in PAX
---------------------

Hook into LINKS-TO-TREE, maybe the ugliest function in the code base,
record links. `*DOCUMENT-INDICES*` would paramaterize the indices:

    (let ((pax:*document-indices* '(((or function macro)
                                     :title "Functions and Macros")
                                    (variable
                                     :title "Variables and Constants")
                                    (type
                                     :title "Types")
                                    (declarations
                                     :title "Declarations")
                                    ((or section glossary-term)
                                     :title "Concepts))
      (pax:document ...))

`pax:*document-indices*` puts the same indices at the end of each of
`pax::@pages`. It gives the "publisher" the same kind of control that
`pax:*document-max-table-of-contents-level*` does.

Implementation is SB-MANUAL
---------------------------

Add support for the `[label override][<section-or-glossary-or-term>]`
syntax. Record the defitions and references at generation time as
specified by `*DOCUMENT-INDICES*`; do not use `@cindex` or other
indexing commands. Output indices at the end without using
`@printindex`.

Well, almost. We want the `i` key in Emacs Info mode to work, so we
need to use Texinfo indices for Info output, which we should be able
to do with `@ifinfo`, `@cindex` & co, and `@printindex`.

Misc
----

`who-documents` and backlinks when live browsing (in "also see")?
