# jps -- jump point search for Haskell

[![Build Status](https://travis-ci.org/isovector/jps.svg?branch=master)](https://travis-ci.org/isovector/jps) | [Hackage][hackage]

[hackage]: https://hackage.haskell.org/package/jps

## Dedication

> People take the longest possible paths, digress to numerous dead ends, and
> make all kinds of mistakes.Then historians come along and write summaries of
> this messy, nonlinear process and make it appear like a simple, straight line.
>
> Dean Kamen


## Overview

Jump point search is a variant of A* that cuts down on the search space by
assuming you always want to continue in a straight line. As such, it runs
remarkably faster on graphs that are mostly open.

For a fantastic introduction to how the algorithm works, check out [zerowidth
positive lookahead][jps]'s excellent explanation.

[jps]: https://zerowidth.com/2013/05/05/jump-point-search-explained.html

`jps` is a Haskell implementation of jump point search. It was originally
written by [Zachary Kamerling][zachary] and is maintained by [Sandy
Maguire][isovector].

[zachary]: https://github.com/ZacharyKamerling
[isovector]: https://github.com/isovector

