# Model/view separation in Standard ML

This package provides primitive facilities for separating view and
model/state in Standard ML. The idea is to have an interactive program
where the user interface remains responsive, even though heavy (and
perhaps blocking) computation is taking place in the underlying model.
This is done by structuring the program as two processes: a view
process and a model process. These communicate via pipes. Some care
must be taken to ensure that the view process is never blocked, as
this would freeze the user interface.

The facilities here are mostly aimed towards (and exclusively tested
with) CLI/TUI interfaces.

## Overview of MLB files

* [lib/github.com/diku-dk/sml-model-view/model-view.mlb](lib/github.com/diku-dk/sml-model-view/model-view.mlb):

  * **signature [CHANNEL](lib/github.com/diku-dk/sml-model-view/channel.sig)** (also the documentation)
  * **structure Channel**

## Usage of the package

The library is designed for use with [smlpkg](https://github.com/diku-dk/smlpkg).

## Trying out the example

```
$ make run
```

Enter lines of input as you wish.
