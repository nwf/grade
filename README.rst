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

  Packaging: [10/10]

   Grader comments:

    Everything looks great here.  Thanks for the very informative README!

  Functionality Tests: [30/40]
  
    (-6)
    The buffer-passing test seems to mangle bytes on occasion.
    
    (-4)
    There is a minor problem with the buffer-passing test output
    when given an unusually long input string.
    
   Grader comments: 
    
    Both of these test failures occur because ...

  Overall: [40/50] (80.0%)

Basic Grader Use
################

The instructor will have prepared a ``skeleton`` file which will look
something like this::

  # Basic features of the handin 
  @packaging
  #:ftbfs_all
  #:missing_readme
  #:readme_no_commentary
  #:readme_no_instructions
  #:tarball_directory
  #:missing_make

  $BEGIN_COMMENTS

  $END_COMMENTS

  # Automated test result section
  @tests
  # Un-comment the appropriate directive for each test failed.
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

Advanced Handling
-----------------

Usually with permission of the instructor (just so there are no surprises),
there are two additional directives that can be used within a section's
directives (not in a comment block):

* ``!pointsadj N`` will adjust the final score by ``N`` (though not lower
  than a total of 0 or more than the section maximum) and should be used
  only sparingly; it is, in general, much better to ask the instructor to
  define a new flag for your case than to make many ad-hoc adjustments.

* ``!pointsset N`` will override the automated score computation within a
  section (to the value ``N``, though the score cannot be set less than 0 or
  more than the section maximum) and should be used very rarely.

These commands may not be mixed in a section.  Multiple ``!pointsadj``
commands will be understood.

Instructor Use
##############

Constructing a Rubric
---------------------

A rubric file, typically called ``defines.conf``, consists of a number of
sections.  Each section has a name as well as some other parameters, and
contains the definition of the flags seen in the grader data.

Sections are introduced with ``@``-lines, like in the grading data, except
that here, they take arguments::

  @section-name type max extra friendly-name

where

* ``section-name`` is the short name as used in the grade data.  It may
  not contain whitespace.

* ``type`` indicates to the ``grade.pl`` script how to interpret this
  section.  If ``type`` begins with ``!``, the section will be omitted
  from the generated skeleton and this initial ``!`` will be stripped
  from the type before consulting the following choices.

  * Type ``0`` defines a section of define flags whose invoked scores are
    simply summed.  ``extra`` is ignored for this type.  This is the only
    type defined at the moment and it must be used in rubric files.

* ``max`` denotes the maximum (and initial) number of points in this section

* ``friendly-name`` is the section heading as presented to students.  It may
  contain spaces, and is in fact the remainder of the @ line.

Within sections, each flag definition takes the form ::

   :flag-name score-modifier
   Commentary paragraph 1
   paragraph 1 line 2

   paragraph 2
   .

``flag-name`` is the name of the flag used in the grading data files.
``score-modifier`` is either 

* a number (as understood by Perl), which adjusts the score of this section
  appropriately.  As such, this number is almost always negative (i.e.,
  beginning with a ``-``), but positive numbers are understood for some form
  of extra credit.  (Note that the script will refuse to set a score higher
  than the section maximum.)

* a ``!`` followed by a number, again, as understood by Perl, usually ``0``.
  Engaging a flag so defined will cause the grading script to act as if a
  ``!pointsset`` directive was encountered.

Text between the line beginning with ``:`` and the dot on a line by itself
will be copied into student grade reports whenever the flag is given in a
grade data file.  In many cases, there are many conditions that may merit
the use of the same flag, and students will benefit from additional feedback
about exactly what offense has been committed; historically, rather than
introduce many flags, a simple "see the note below" in the prose has
sufficed.  Of course, this is up to judgement and taste.

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
  @tests 0 40 - Functionality Tests
  # Un-comment the appropriate directive for each test failed.

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

  ./make-skeleton.pl < defines.conf > skeleton

Producing grade results
-----------------------

Given a rubric and a grader data file, ``student.data``, one runs ::

  ./grade.pl defines.conf < student.data

to obtain the pretty-printed report and numeric score result.

It is easy to adjust the weights of different flags and sections by simply
altering the values in ``defines.conf`` file and re-running the ``grade.pl``
script.
