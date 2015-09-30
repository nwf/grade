##########################################
GRADE Reporting And Definition Environment
##########################################

Inspired by the grading infrastructure developed by David Eckhardt at
Carnegie Mellon University for http://www.cs.cmu.edu/~410/ , GRADE is a
lightweight, file-based, *directive*-based approach to managing the work of
grading students in a course.  It uses a minimal markup format for both its
machine-readable scoring rubric and for grader data files.

Student View
############

Students will get back tastefully formatted grade files.  A very short
example looks something like this::

  Packaging: [10/10] (100.00%)

   Grader comments:

    Everything looks great here.  Thanks for the very informative README!

  Functionality Tests: [30/40] (75.00%)
  
    (-6.0)
    The buffer-passing test seems to mangle bytes on occasion.
    
    (-4.0)
    There is a minor problem with the buffer-passing test output
    when given an unusually long input string.
    
   Grader comments: 
    
    Both of these test failures occur because ...

  TOTAL: [40/50] (80.00%)

Basic Grader Use
################

The instructor will have prepared a ``skeleton`` file which will look
something like this::

  # Basic features of the handin 
  #@packaging
   #:ftbfs_all
   #:missing_readme
   #:readme_no_commentary
   #:readme_no_instructions
   #:tarball_directory
   #:missing_make

  $BEGIN_COMMENTS

  $END_COMMENTS

  # Automated test result section
  # Un-comment the appropriate directive for each test failed.
  #@tests
   #:ftbfs_all
   #:simple_test
   #:more_interesting_test
   #:more_interesting_test_minor
   #:test_everything

  $BEGIN_COMMENTS

  $END_COMMENTS

etc.  The graders should copy this skeleton and mutate it appropriately for
each assignment they grade.  The example grade output above might have come
from a grading data file like this::

  @packaging
  $BEGIN_COMMENTS

  Everything looks great here.  Thanks for the very informative README!

  $END_COMMENTS

  @tests
  :simple_test
  :more_interesting_test_minor

  $BEGIN_COMMENTS

  Both of these test failures occur because ...

  $END_COMMENTS

In more detail:

* Sections are separated by lines beginning with ``@``.

* Blocks surrounded by ``$BEGIN_COMMENTS``/``$END_COMMENTS`` will be copied
  into the appropriate section of the student report under the heading of
  ``Grader comments``.

* **outside** the comment blocks, all characters on a line after a ``#``
  mark will be ignored.  Inside comment blocks, all bytes will be copied
  over verbatim (modulo an indentation prefix for pretty-printing).

* Text and scores associated with each un-commented ``:``-line (which are
  defined by the instructor) will also be emitted to the correct section.

Instructor Use
##############

Constructing a Rubric
---------------------

A rubric file, typically called ``defines.conf``, consists of a number of
sections.  Each section has a name as well as some other parameters, and
contains the definition of the flags seen in the grader data.

Sections are introduced with ``@``-lines, like in the grading data, except
that here, they take arguments::

  @section-name type [extra] - friendly-name

where

* ``section-name`` is the short name as used in the grade data.  It may
  not contain whitespace.

* ``type`` indicates to the ``grade`` program how to interpret this
  section.  If ``type`` begins with ``!``, the section will be omitted
  from the generated skeleton and this initial ``!`` will be stripped
  from the type before consulting the following choices.

  There are a few base-case scoring engines:

    * The word ``simple`` defines a section of define flags whose invoked
      scores are simply summed.  ``extra`` here should be the section's
      maximum value.

    * The word ``equal`` defines a section of equally-weighted flags; again,
      ``extra`` should be the section's maximum value.

    * The word ``seconly`` defines a section with no flags (unless possibly
      combined with one of the modifiers below) but instead just takes a
      literal score after the ``@section`` line in the data file.  (The
      parser also accepts a literal ``!`` in this position, in which case it
      will raise an error unless some modifier below overrides the score;
      this facilitates common rationale text even in this kind of section.)

  There are, additionally, some modifiers available:

    * The word ``bounding`` followed by (whitespace and) another ``type``
      will behave as that type except that the score will be between zero
      and that type's derived maximum.  That is, this section will behave as
      if it had that ``type`` but will yield no scores below zero and no
      extra credit.

    * The word ``nonneg`` behaves like ``bounding`` but bounds the section's
      score from below at ``0``; that is, it permits extra credit but not
      extra loss.

    * The word ``commenting`` followed by (whitespace and) another ``type``
      will permit the definitions of flags with argument ``!C`` which will
      not influence the score at all and will not print out a score modifier
      before the flag text in generated reports.

    * The word ``zeroing`` followed by (whitespace and) another ``type``
      will permit the definitions of flags with argument ``!0`` which will
      set the section score to zero.

  Some shorthands are defined:

    * The word ``0`` is a shorthand for ``zeroing bounding commenting simple``.

* ``friendly-name`` is the section heading as presented to students.  It may
  contain spaces, and is in fact the remainder of the @ line.

Within sections, each flag definition takes the form ::

   :flag-name score-modifier
   Commentary paragraph 1
   paragraph 1 line 2

   paragraph 2
   .

``flag-name`` is the name of the flag used in the grading data files.
``score-modifier`` is defined by the section type.  While most flags will be
introduced with ``:``, it is also possible to use ``;``; flags defined this
later way are OK for *multi-use* (with the commensurate impact on the
section's score), whereas ``:``-defined flags will trigger an error if used
more than once.

* For ``simple`` sections, the ``score-modifier`` may be

  * a number, which adjusts the score of this section by that many absolute
    points.  As such, this number is almost always negative (i.e.,
    beginning with a ``-``), but positive numbers are understood for some form
    of extra credit.  (Note that the script will refuse to set a score higher
    than the section maximum.)

  * a number followed by a ``%`` character, which will adjust the section
    score by that percentage of the maximum number of points available in
    the section.

  * The literal string ``!0``.  Engaging any whole number of flags so
    defined will set the section's score to zero.

* For ``equal`` sections, the only permitted non-empty ``score-modifier``
  is ``!0``, which is interpreted as in ``simple`` sections.  All other
  flags in this type of section should have an empty ``score-modifier``.

Text between the line beginning with ``:`` (or ``;``) and the dot on a line
by itself will be copied into student grade reports whenever the flag is
given in a grade data file.  In many cases, there are many conditions that
may merit the use of the same flag, and students will benefit from
additional feedback about exactly what offense has been committed;
historically, rather than introduce many flags, a simple "see the note
below" in the prose has sufficed.  Of course, this is up to judgement and
taste.

Lines that begin with ``#`` and not ``#!`` will be copied into the skeleton.
Lines beginning with ``#!`` will be ignored entirely, except for some
additional advanced handling:

* ``#!\n`` (yes, a literal backslash) will cause an empty line to be emitted
  into the skeleton if the containing section is not being skipped.

* ``#!noskip`` will cause subsequent comment lines in a skipped section to
  to be emitted.  In a non-skipped section, it has no effect.

* ``#!reskip`` will cause subsequent comment lines to be skipped if the
  containing section is skipped.  It has no effect otherwise.

Continuing the example above, the corresponding ``defines.conf`` contains,
among other defines ::

  #! This line will be ignored
  # Basic features of the handin 
  @packaging 0 10 - Packaging
  :ftbfs_all -10
  The submission failed to compile.
  .

  #! ...

  # Automated test result section
  # Un-comment the appropriate directive for each test failed.
  @tests 0 40 - Functionality Tests

  #! ...

  :simple_test -6
  The buffer-passing test seems to mangle bytes on occasion.
  .

  #! ...
   
  :more_interesting_test_minor -4
  There is a minor problem with the buffer-passing test output
  when given an unusually long input string.
  .

  #! ...

Generating a Skeleton
---------------------

Given a rubric, typically called ``defines.conf``, one can produce a
skeletal grading file by ::

  grade make-skeleton < defines.conf > skeleton

Producing grade results
-----------------------

Given a rubric and a grader data file, ``student.data``, one runs ::

  grade grade-one defines.conf < student.data

to obtain the pretty-printed report and numeric score result.

It is easy to adjust the weights of different flags and sections by simply
altering the values in ``defines.conf`` file and re-running the ``grade``
program.
