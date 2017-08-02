cmark-hs
========

This package provides Haskell bindings for [libcmark], the reference
parser for [CommonMark], a fully specified variant of Markdown.
It includes sources for [libcmark] and does not require prior
installation of the C library.

cmark provides the following advantages over existing Markdown
libraries for Haskell:

  - **Speed:** cmark can render a Markdown version of *War and Peace* in
    the blink of an eye.  Conversion speed is on par with the
    [sundown] library, though we were unable to benchmark precisely,
    because [sundown] raised a malloc error when compiled into our
    benchmark suite. Relative to other Haskell Markdown libraries:
    cmark was 82 times faster than [cheapskate], 59 times faster than
    [markdown], 105 times faster than [pandoc], and 3 times faster
    than [discount].

  - **Memory footprint:**  Memory footprint is on par with [sundown].
    On one sample, the library uses a fourth the memory that [markdown]
    uses, and less than a tenth the memory that [pandoc] uses.

  - **Robustness:**  cmark can handle whatever is thrown at it,
    without the exponential blowups in parsing time that sometimes afflict
    other libraries.  (The input `bench/full-sample.md`,
    for example, causes both [pandoc] and [markdown] to grind to a
    halt.)  [libcmark] has been extensively fuzz-tested.

  - **Accuracy:**  cmark passes the CommonMark spec's suite of over
    600 conformance tests.

  - **Standardization:**  Since there is a spec and a comprehensive suite
    of tests, we can have a high degree of confidence that any two
    CommonMark implementations will behave the same.  Thus, for
    example, one could use this library for server-side rendering
    and [commonmark.js] for client-side previewing.

  - **Multiple renderers.**  Output in HTML, groff man, LaTeX, CommonMark,
    and a custom XML format is supported. And it is easy to write new
    renderers to support other formats.

  - **Ease of installation:** cmark is portable and has minimal
    dependencies.

cmark does not provide Haskell versions of the whole [libcmark]
API, which is built around mutable `cmark_node` objects.  Instead, it
provides functions for converting CommonMark to HTML (and other
formats), and a function for converting CommonMark to a `Node`
tree that can be processed further using Haskell.

**A note on security:**  This library does not attempt to sanitize
HTML output.  We recommend using [xss-sanitize] to filter the output,
or enabling `optSafe` to filter out all raw HTML and potentially
dangerous URLs.

**A note on stability:**  There is a good chance the API will change
significantly after this early release.

[CommonMark]: http://commonmark.org
[libcmark]: http://github.com/jgm/cmark
[benchmarks]: https://github.com/jgm/cmark/blob/master/benchmarks.md
[cheapskate]: https://hackage.haskell.org/package/cheapskate
[pandoc]: https://hackage.haskell.org/package/pandoc
[sundown]: https://hackage.haskell.org/package/sundown
[markdown]: https://hackage.haskell.org/package/markdown
[commonmark.js]: http://github.com/jgm/commonmark.js
[xss-sanitize]: https://hackage.haskell.org/package/xss-sanitize
[discount]: https://hackage.haskell.org/package/discount
