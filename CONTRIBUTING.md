# Contributing

First of all, thanks for your interest in contributing to Finkel!

We want to make contributing to this project as easy and transparent
as possible, whether it's:

- Reporting a bug
- Discussing the current state of the code
- Submitting a fix
- Proposing new features
- Becoming a maintainer

Finkel is an open source project. Following these guidelines helps to
communicate that you respect the time of the developers managing and
developing this open source project. In return, they should
reciprocate that respect in addressing your issue, assessing changes,
and helping you finalize your pull requests.


## Code of Conduct

All members of our community are expected to follow our [Code of
Conduct][coc]. Please make sure you are welcoming and friendly in all
of our spaces.

[coc]: https://github.com/finkel-lang/finkel/blob/master/CODE_OF_CONDUCT.md


## Getting started with Finkel source

Please see the [Building And Installing][doc-install] section of the
documentation for detailed instruction. In short:

```
$ git clone https://github.com/finkel-lang/finkel
$ cd finkel
$ stack build
```

[doc-install]: https://finkel.readthedocs.io/en/latest/contents/install.html


## Issues

We use the [github issue tracker][ghissue] to manage issues. Please do
some searches in the existing issues before creating a new one. When
sending a bug report, please make sure that you are using the latest
version of Finkel built from the source.

[ghissue]: https://github.com/finkel-lang/finkel/issues


## Pull Requests

### Style guide / Coding conventions

We believe every one has its taste in coding style. However, the
following provides some suggestions:

- By default, all source codes are written with max 75 characters per
  line, but there are some exceptions, e.g. use of long string
  constants in URL.

- For Haskell source code, use 2 spaces for indentations. We use
  [hlint][hlint] and [stylish-haskell][stylish-haskell] with the
  configuration files in the repository root directory. It is totally
  fine to make changes to the configuration files, just please tell us
  why.

- For Finkel source code, not much to say at the moment, since the
  language is still young. However, try to follow the style used in
  the file when you modify the existing file.

- Please consider writing [a good Git commit message][gitcommit].

[hlint]: https://github.com/ndmitchell/hlint
[stylish-haskell]: https://github.com/jaspervdj/stylish-haskell
[gitcommit]: https://chris.beams.io/posts/git-commit/#seven-rules

### Running tests

Please make sure that the tests are passing with your modifications.
For example, to test with [stack][stack], run:

```
$ stack build --test
```

[stack]: https://docs.haskellstack.org/en/stable/README/

### Trivial changes

Small contributions such as fixing spelling errors, can be also
submitted by a contributor as a pull request.

As a rule of thumb, changes are obvious fixes if they do not introduce
any new functionality or creative thinking. As long as the change does
not affect functionality, some likely examples include the following:

- Spelling/grammar fixes
- Typo correction, white space and formatting changes
- Comment clean up
- Changes to *metadata* files like ``.gitignore``, etc.

### License

In short, when you submit code changes, your submissions are
understood to be under the same [BSD 3-clause License][bsd3] that
covers the project.

[bsd3]: https://choosealicense.com/licenses/bsd-3-clause/
