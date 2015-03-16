CommonMark
==========

CommonMark is a rationalized version of Markdown syntax,
with a [spec][the spec] and BSD3-licensed reference
implementations in C and JavaScript.

[Try it now!](http://spec.commonmark.org/dingus.html)

The implementations
-------------------

The C implementation provides both a shared library (`libcmark`) and a
standalone program `cmark` that converts CommonMark to HTML.  It is
written in standard C99 and has no library dependencies.  The parser is
very fast (see [benchmarks](benchmarks.md)).

It is easy to use `libcmark` in python, lua, ruby, and other dynamic
languages: see the `wrappers/` subdirectory for some simple examples.

The JavaScript implementation provides both an NPM package and a
single JavaScript file, with no dependencies, that can be linked into
an HTML page. For further information, see the
[README in the js directory](js/README.md).

**A note on security:**
Neither implementation attempts to sanitize link attributes or
raw HTML.  If you use these libraries in applications that accept
untrusted user input, you must run the output through an HTML
sanitizer to protect against
[XSS attacks](http://en.wikipedia.org/wiki/Cross-site_scripting).

Installing (C)
--------------

Building the C program (`cmark`) and shared library (`libcmark`)
requires [cmake].  If you modify `scanners.re`, then you will also
need [re2c], which is used to generate `scanners.c` from
`scanners.re`.  We have included a pre-generated `scanners.c` in
the repository to reduce build dependencies.

If you have GNU make, you can simply `make`, `make test`, and `make
install`.  This calls [cmake] to create a `Makefile` in the `build`
directory, then uses that `Makefile` to create the executable and
library.  The binaries can be found in `build/src`.

For a more portable method, you can use [cmake] manually. [cmake] knows
how to create build environments for many build systems.  For example,
on FreeBSD:

    mkdir build
    cd build
    cmake ..  # optionally: -DCMAKE_INSTALL_PREFIX=path
    make      # executable will be created as build/src/cmark
    make test
    make install

Or, to create Xcode project files on OSX:

    mkdir build
    cd build
    cmake -G Xcode ..
    make
    make test
    make install

The GNU Makefile also provides a few other targets for developers.
To run a benchmark:

    make bench

To run a "fuzz test" against ten long randomly generated inputs:

    make fuzztest

To run a test for memory leaks using `valgrind`:

    make leakcheck

To reformat source code using `astyle`:

    make astyle

To make a release tarball and zip archive:

    make archive


Compiling for Windows
---------------------

To compile with MSVC and NMAKE:

    nmake

You can cross-compile a Windows binary and dll on linux if you have the
`mingw32` compiler:

    make mingw

The binaries will be in `build-mingw/windows/bin`.

Installing (JavaScript)
-----------------------

The JavaScript library can be installed through `npm`:

    npm install commonmark

This includes a command-line converter called `commonmark`.

If you want to use it in a client application, you can fetch
a pre-built copy of `commonmark.js` from
<http://spec.commonmark.org/js/commonmark.js>.

For further information, see the
[README in the js directory](js/README.md).

The spec
--------

[The spec] contains over 500 embedded examples which serve as conformance
tests. To run the tests using an executable `$PROG`:

    python3 test/spec_tests.py --program $PROG

If you want to extract the raw test data from the spec without
actually running the tests, you can do:

    python3 test/spec_tests.py --dump-tests

and you'll get all the tests in JSON format.

[The spec]:  http://spec.commonmark.org/0.13/

The source of [the spec] is `spec.txt`.  This is basically a Markdown
file, with code examples written in a shorthand form:

    .
    Markdown source
    .
    expected HTML output
    .

To build an HTML version of the spec, do `make spec.html`.  To build a
PDF version, do `make spec.pdf`.  (Creating a PDF requires [pandoc]
and a LaTeX installation.  Creating the HTML version requires only
`libcmark` and `python3`.)

The spec is written from the point of view of the human writer, not
the computer reader.  It is not an algorithm---an English translation of
a computer program---but a declarative description of what counts as a block
quote, a code block, and each of the other structural elements that can
make up a Markdown document.

Because John Gruber's [canonical syntax
description](http://daringfireball.net/projects/markdown/syntax) leaves
many aspects of the syntax undetermined, writing a precise spec requires
making a large number of decisions, many of them somewhat arbitrary.
In making them, we have appealed to existing conventions and
considerations of simplicity, readability, expressive power, and
consistency.  We have tried to ensure that "normal" documents in the many
incompatible existing implementations of Markdown will render, as far as
possible, as their authors intended.  And we have tried to make the rules
for different elements work together harmoniously.  In places where
different decisions could have been made (for example, the rules
governing list indentation), we have explained the rationale for
my choices.  In a few cases, we have departed slightly from the canonical
syntax description, in ways that we think further the goals of Markdown
as stated in that description.

For the most part, we have limited ourselves to the basic elements
described in Gruber's canonical syntax description, eschewing extensions
like footnotes and definition lists.  It is important to get the core
right before considering such things. However, we have included a visible
syntax for line breaks and fenced code blocks.

Differences from original Markdown
----------------------------------

There are only a few places where this spec says things that contradict
the canonical syntax description:

-   It allows all punctuation symbols to be backslash-escaped,
    not just the symbols with special meanings in Markdown. We found
    that it was just too hard to remember which symbols could be
    escaped.

-   It introduces an alternative syntax for hard line
    breaks, a backslash at the end of the line, supplementing the
    two-spaces-at-the-end-of-line rule. This is motivated by persistent
    complaints about the “invisible” nature of the two-space rule.

-   Link syntax has been made a bit more predictable (in a
    backwards-compatible way). For example, `Markdown.pl` allows single
    quotes around a title in inline links, but not in reference links.
    This kind of difference is really hard for users to remember, so the
    spec allows single quotes in both contexts.

-   The rule for HTML blocks differs, though in most real cases it
    shouldn't make a difference. (See the section on HTML Blocks
    for details.) The spec's proposal makes it easy to include Markdown
    inside HTML block-level tags, if you want to, but also allows you to
    exclude this. It is also makes parsing much easier, avoiding
    expensive backtracking.

-   It does not collapse adjacent bird-track blocks into a single
    blockquote:

        > this is two

        > blockquotes

        > this is a single
        >
        > blockquote with two paragraphs

-   Rules for content in lists differ in a few respects, though (as with
    HTML blocks), most lists in existing documents should render as
    intended. There is some discussion of the choice points and
    differences in the subsection of List Items entitled Motivation.
    We think that the spec's proposal does better than any existing
    implementation in rendering lists the way a human writer or reader
    would intuitively understand them. (We could give numerous examples
    of perfectly natural looking lists that nearly every existing
    implementation flubs up.)

-   The spec stipulates that two blank lines break out of all list
    contexts.  This is an attempt to deal with issues that often come up
    when someone wants to have two adjacent lists, or a list followed by
    an indented code block.

-   Changing bullet characters, or changing from bullets to numbers or
    vice versa, starts a new list. We think that is almost always going
    to be the writer's intent.

-   The number that begins an ordered list item may be followed by
    either `.` or `)`. Changing the delimiter style starts a new
    list.

-   The start number of an ordered list is significant.

-   Fenced code blocks are supported, delimited by either
    backticks (```` ``` ```` or tildes (` ~~~ `).

Contributing
------------

There is a [forum for discussing
CommonMark](http://talk.commonmark.org); you should use it instead of
github issues for questions and possibly open-ended discussions.
Use the [github issue tracker](http://github.com/jgm/CommonMark/issues)
only for simple, clear, actionable issues.

Authors
-------

The spec was written by John MacFarlane, drawing on

- his experience writing and maintaining Markdown implementations in several
  languages, including the first Markdown parser not based on regular
  expression substitutions ([pandoc](http://github.com/jgm/pandoc)) and
  the first markdown parsers based on PEG grammars
  ([peg-markdown](http://github.com/jgm/peg-markdown),
  [lunamark](http://github.com/jgm/lunamark))
- a detailed examination of the differences between existing Markdown
  implementations using [BabelMark 2](http://johnmacfarlane.net/babelmark2/),
  and
- extensive discussions with David Greenspan, Jeff Atwood, Vicent
  Marti, Neil Williams, and Benjamin Dumke-von der Ehe.

John MacFarlane was also responsible for the original versions of the
C and JavaScript implementations.  The block parsing algorithm was
worked out together with David Greenspan.  Vicent Marti
optimized the C implementation for performance, increasing its speed
tenfold.  Kārlis Gaņģis helped work out a better parsing algorithm
for links and emphasis, eliminating several worst-case performance
issues.  Nick Wellnhofer contributed many improvements, including
most of the C library's API and its test harness.  Vitaly Puzrin
has offered much good advice about the JavaScript implementation.

[cmake]: http://www.cmake.org/download/
[pandoc]: http://johnmacfarlane.net/pandoc/
[re2c]: http://re2c.org

> the simple example of a blockquote 
> the simple example of a blockquote
> the simple example of a blockquote
> the simple example of a blockquote
... continuation
... continuation
... continuation
... continuation

empty blockquote:

>
>
>
>

>>>>>> deeply nested blockquote
>>>>> deeply nested blockquote
>>>> deeply nested blockquote
>>> deeply nested blockquote
>> deeply nested blockquote
> deeply nested blockquote

> deeply nested blockquote
>> deeply nested blockquote
>>> deeply nested blockquote
>>>> deeply nested blockquote
>>>>> deeply nested blockquote
>>>>>> deeply nested blockquote

        an
        example

        of



        a code
        block


``````````text
an
example
```
of


a fenced
```
code
block
``````````

# heading
### heading
##### heading

# heading #
### heading ###
##### heading \#\#\#\#\######

############ not a heading

 * * * * *

 -  -  -  -  -

 ________


 ************************* text

<div class="this is an html block">

blah blah

</div>

<table>
  <tr>
    <td>
      **test**
    </td>
  </tr>
</table>

<table>

  <tr>

    <td>

      test

    </td>

  </tr>

</table>

<![CDATA[
  [[[[[[[[[[[... *cdata section - this should not be parsed* ...]]]]]]]]]]]
]]>

heading
---

heading
===================================

not a heading
----------------------------------- text
 - tidy
 - bullet
 - list


 - loose

 - bullet

 - list


 0. ordered
 1. list
 2. example


 -
 -
 -
 -


 1.
 2.
 3.


 -  an example
of a list item
       with a continuation

    this part is inside the list

   this part is just a paragraph  


 1. test
 -  test
 1. test
 -  test


111111111111111111111111111111111111111111. is this a valid bullet?

 - _________________________

 - this
 - is

   a

   long
 - loose
 - list

 - with
 - some

   tidy

 - list
 - items
 - in

 - between
 - _________________________

 - this
   - is
     - a
       - deeply
         - nested
           - bullet
             - list
   

 1. this
    2. is
       3. a
          4. deeply
             5. nested
                6. unordered
                   7. list


 - 1
  - 2
   - 3
    - 4
     - 5
      - 6
       - 7
      - 6
     - 5
    - 4
   - 3
  - 2
 - 1


 - - - - - - - - - deeply-nested one-element item

[1] [2] [3] [1] [2] [3]

[looooooooooooooooooooooooooooooooooooooooooooooooooong label]

 [1]: <http://something.example.com/foo/bar>
 [2]: http://something.example.com/foo/bar 'test'
 [3]:
 http://foo/bar
 [    looooooooooooooooooooooooooooooooooooooooooooooooooong   label    ]:
 111
 'test'
 [[[[[[[[[[[[[[[[[[[[ this should not slow down anything ]]]]]]]]]]]]]]]]]]]]: q
 (as long as it is not referenced anywhere)

 [[[[[[[[[[[[[[[[[[[[]: this is not a valid reference
[[[[[[[foo]]]]]]]

[[[[[[[foo]]]]]]]: bar
[[[[[[foo]]]]]]: bar
[[[[[foo]]]]]: bar
[[[[foo]]]]: bar
[[[foo]]]: bar
[[foo]]: bar
[foo]: bar

[*[*[*[*[foo]*]*]*]*]

[*[*[*[*[foo]*]*]*]*]: bar
[*[*[*[foo]*]*]*]: bar
[*[*[foo]*]*]: bar
[*[foo]*]: bar
[foo]: bar
closed (valid) autolinks:

 <ftp://1.2.3.4:21/path/foo>
 <http://foo.bar.baz?q=hello&id=22&boolean>
 <http://veeeeeeeeeeeeeeeeeeery.loooooooooooooooooooooooooooooooong.autolink/>
 <teeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeest@gmail.com>

these are not autolinks:

 <ftp://1.2.3.4:21/path/foo
 <http://foo.bar.baz?q=hello&id=22&boolean
 <http://veeeeeeeeeeeeeeeeeeery.loooooooooooooooooooooooooooooooong.autolink
 <teeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeest@gmail.com
 < http://foo.bar.baz?q=hello&id=22&boolean >
`lots`of`backticks`

``i``wonder``how``this``will``be``parsed``
*this* *is* *your* *basic* *boring* *emphasis*

_this_ _is_ _your_ _basic_ _boring_ _emphasis_

**this** **is** **your** **basic** **boring** **emphasis**
*this *is *a *bunch* of* nested* emphases* 

__this __is __a __bunch__ of__ nested__ emphases__ 

***this ***is ***a ***bunch*** of*** nested*** emphases*** 
*this *is *a *worst *case *for *em *backtracking

__this __is __a __worst __case __for __em __backtracking

***this ***is ***a ***worst ***case ***for ***em ***backtracking
entities:

&nbsp; &amp; &copy; &AElig; &Dcaron; &frac34; &HilbertSpace; &DifferentialD; &ClockwiseContourIntegral;

&#35; &#1234; &#992; &#98765432;

non-entities:

&18900987654321234567890; &1234567890098765432123456789009876543212345678987654;

&qwertyuioppoiuytrewqwer; &oiuytrewqwertyuioiuytrewqwertyuioytrewqwertyuiiuytri;

\t\e\s\t\i\n\g \e\s\c\a\p\e \s\e\q\u\e\n\c\e\s

\!\\\"\#\$\%\&\'\(\)\*\+\,\.\/\:\;\<\=\>\?

\@ \[ \] \^ \_ \` \{ \| \} \~ \- \'

\
\\
\\\
\\\\
\\\\\

\<this\> \<is\> \<not\> \<html\>

Taking commonmark tests from the spec for benchmarking here:

<a><bab><c2c>

<a/><b2/>

<a  /><b2
data="foo" >

<a foo="bar" bam = 'baz <em>"</em>'
_boolean zoop:33=zoop:33 />

<33> <__>

<a h*#ref="hi">

<a href="hi'> <a href=hi'>

< a><
foo><bar/ >

<a href='bar'title=title>

</a>
</foo >

</a href="foo">

foo <!-- this is a
comment - with hyphen -->

foo <!-- not a comment -- two hyphens -->

foo <?php echo $a; ?>

foo <!ELEMENT br EMPTY>

foo <![CDATA[>&<]]>

<a href="&ouml;">

<a href="\*">

<a href="\"">
Valid links:

 [this is a link]()
 [this is a link](<http://something.example.com/foo/bar>)
 [this is a link](http://something.example.com/foo/bar 'test')
 ![this is an image]()
 ![this is an image](<http://something.example.com/foo/bar>)
 ![this is an image](http://something.example.com/foo/bar 'test')
 
 [escape test](<\>\>\>\>\>\>\>\>\>\>\>\>\>\>> '\'\'\'\'\'\'\'\'\'\'\'\'\'\'')
 [escape test \]\]\]\]\]\]\]\]\]\]\]\]\]\]\]\]](\)\)\)\)\)\)\)\)\)\)\)\)\)\))

Invalid links:

 [this is not a link

 [this is not a link](

 [this is not a link](http://something.example.com/foo/bar 'test'
 
 [this is not a link](((((((((((((((((((((((((((((((((((((((((((((((
 
 [this is not a link]((((((((((()))))))))) (((((((((()))))))))))
Valid links:

[[[[[[[[](test)](test)](test)](test)](test)](test)](test)]

[ [[[[[[[[[[[[[[[[[[ [](test) ]]]]]]]]]]]]]]]]]] ](test)

Invalid links:

[[[[[[[[[

[ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [ [

![![![![![![![![![![![![![![![![![![![![![![![![![![![![![![![![![![![![![![

this\
should\
be\
separated\
by\
newlines

this  
should  
be  
separated  
by  
newlines  
too

this
should
not
be
separated
by
newlines

Lorem ipsum dolor sit amet, __consectetur__ adipiscing elit. Cras imperdiet nec erat ac condimentum. Nulla vel rutrum ligula. Sed hendrerit interdum orci a posuere. Vivamus ut velit aliquet, mollis purus eget, iaculis nisl. Proin posuere malesuada ante. Proin auctor orci eros, ac molestie lorem dictum nec. Vestibulum sit amet erat est. Morbi luctus sed elit ac luctus. Proin blandit, enim vitae egestas posuere, neque elit ultricies dui, vel mattis nibh enim ac lorem. Maecenas molestie nisl sit amet velit dictum lobortis. Aliquam erat volutpat.

Vivamus sagittis, diam in [vehicula](https://github.com/markdown-it/markdown-it) lobortis, sapien arcu mattis erat, vel aliquet sem urna et risus. Ut feugiat sapien vitae mi elementum laoreet. Suspendisse potenti. Aliquam erat nisl, aliquam pretium libero aliquet, sagittis eleifend nunc. In hac habitasse platea dictumst. Integer turpis augue, tincidunt dignissim mauris id, rhoncus dapibus purus. Maecenas et enim odio. Nullam massa metus, varius quis vehicula sed, pharetra mollis erat. In quis viverra velit. Vivamus placerat, est nec hendrerit varius, enim dui hendrerit magna, ut pulvinar nibh lorem vel lacus. Mauris a orci iaculis, hendrerit eros sed, gravida leo. In dictum mauris vel augue varius, ac ullamcorper nisl ornare. In eu posuere velit, ac fermentum arcu. Interdum et malesuada fames ac ante ipsum primis in faucibus. Nullam sed malesuada leo, at interdum elit.

Nullam ut tincidunt nunc. [Pellentesque][1] metus lacus, commodo eget justo ut, rutrum varius nunc. Sed non rhoncus risus. Morbi sodales gravida pulvinar. Duis malesuada, odio volutpat elementum vulputate, massa magna scelerisque ante, et accumsan tellus nunc in sem. Donec mattis arcu et velit aliquet, non sagittis justo vestibulum. Suspendisse volutpat felis lectus, nec consequat ipsum mattis id. Donec dapibus vehicula facilisis. In tincidunt mi nisi, nec faucibus tortor euismod nec. Suspendisse ante ligula, aliquet vitae libero eu, vulputate dapibus libero. Sed bibendum, sapien at posuere interdum, libero est sollicitudin magna, ac gravida tellus purus eu ipsum. Proin ut quam arcu.

Suspendisse potenti. Donec ante velit, ornare at augue quis, tristique laoreet sem. Etiam in ipsum elit. Nullam cursus dolor sit amet nulla feugiat tristique. Phasellus ac tellus tincidunt, imperdiet purus eget, ullamcorper ipsum. Cras eu tincidunt sem. Nullam sed dapibus magna. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In id venenatis tortor. In consectetur sollicitudin pharetra. Etiam convallis nisi nunc, et aliquam turpis viverra sit amet. Maecenas faucibus sodales tortor. Suspendisse lobortis mi eu leo viverra volutpat. Pellentesque velit ante, vehicula sodales congue ut, elementum a urna. Cras tempor, ipsum eget luctus rhoncus, arcu ligula fermentum urna, vulputate pharetra enim enim non libero.

Proin diam quam, elementum in eleifend id, elementum et metus. Cras in justo consequat justo semper ultrices. Sed dignissim lectus a ante mollis, nec vulputate ante molestie. Proin in porta nunc. Etiam pulvinar turpis sed velit porttitor, vel adipiscing velit fringilla. Cras ac tellus vitae purus pharetra tincidunt. Sed cursus aliquet aliquet. Cras eleifend commodo malesuada. In turpis turpis, ullamcorper ut tincidunt a, ullamcorper a nunc. Etiam luctus tellus ac dapibus gravida. Ut nec lacus laoreet neque ullamcorper volutpat.

Nunc et leo erat. Aenean mattis ultrices lorem, eget adipiscing dolor ultricies eu. In hac habitasse platea dictumst. Vivamus cursus feugiat sapien quis aliquam. Mauris quam libero, porta vel volutpat ut, blandit a purus. Vivamus vestibulum dui vel tortor molestie, sit amet feugiat sem commodo. Nulla facilisi. Sed molestie arcu eget tellus vestibulum tristique.

[1]: https://github.com/markdown-it

this is a test for tab expansion, be careful not to replace them with spaces

1	4444
22	333
333	22
4444	1


	tab-indented line
    space-indented line
	tab-indented line


a lot of                                                spaces in between here

a lot of												tabs in between here

