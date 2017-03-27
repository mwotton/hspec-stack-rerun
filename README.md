DEPRECATED
==========

HSpec has these features built-in now: just run

```
stack build --file-watch --test --test-arguments '--rerun --failure-report=TESTREPORT --rerun-all-on-success'
```

hspec-stack-rerun
=================

this is a little utility for taking advantage of hspec's rerun capabilities
from within stack --file-watch. Waiting for all your tests to pass
when you're drilled in is a huge waste of time, and you fall out of
the groove (or end up making a coffee) a lot of the time. hspec
--rerun is great for this, as it keeps track of the currently failing
tests and runs them before doing a full rerun.

stack build --test --file-watch is similarly great, as it watches all
the important files in your project (including any on-disk
dependencies), and rebuilds when necessary.

Unfortunately, hspec --rerun only works within ghci, as hspec saves
the failed tests in an environment variable, and stack operates by
building a new binary each time. This combines the best of both, by
stashing the failed tests in a file you specify.

Background
==========

this was inspired by
https://github.com/commercialhaskell/stack/issues/1928

The ghcid-based solution on that page works and is very fast, but if
you have any on-disk dependencies in your stack.yaml, ghcid will not
notice that they have changed and reload them, so you end up killing
ghcid in order to rebuild with stack periodically, and it breaks your
flow (not to mention having to run the whole test suite again to get
started)

Usage
=====

After adding hspec-stack-rerun to your cabal file, change your top
level spec so that it's just a file exporting 'spec', rather than a
Main module. You can do this automatically with

```
{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Spec  #-}
```

or write the whole thing out by hand if you want to drive stick.

Then, your actual test Main.hs should look like this:

```
import qualified.Spec
import qualified Rerun

main = Rerun.main Spec.main
```

Finally, you have to define a file to stash the results in. This is
passed in as an environment variable.

```
HSPEC_FAILURES_FILE=`pwd`/testwatch_canary  stack build --test --test-arguments '--rerun' --file-watch --fast
```

(You will probably have to pass in a shared STACK_YAML too.)
