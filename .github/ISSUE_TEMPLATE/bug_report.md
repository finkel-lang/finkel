---
name: Bug Report
about: Report a bug in Finkel
---

Please follow the steps below for reporting a bug:

Make sure that you are using the latest source (currently git HEAD in
the master branch).

Please use the following schema for your bug report:

### General summary/comments (optional)

### Steps to reproduce

For example:

1. Remove directory *foo*.
2. Run command `finkel bar`.
3. Edit file buzz.
4. Run command `finkel quux`.

### Expected

What you expected to see and happen.

### Actual

What actually happened.

If you suspect that a finkel command misbehaved, please include the
output of that command in `debug` mode.  If the output is larger than
a page please paste the output in a [Gist](https://gist.github.com/).

```
$ FNK_DEBUG=1 finkel <your command here> <args>
<output>
```

### Finkel version and environment information

Finkel version could be obtained with below command.

```
$ finkel version
<output>
```

* OS name and version
* ... etc

### Method of installation

* Via cabal-install
* Via stack
* Other (please specify)
