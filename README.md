# Model/view separation in Standard ML

This repository demonstrates how to separate view and model/state in
Standard ML. The idea is to have an interactive program where the user
interface remains responsive, even though heavy (and perhaps blocking)
computation is taking place in the underlying model. This is done by
structuring the program as two processes: a view process and a model
process. These communicate via pipes. Some care must be taken to
ensure that the view process is never blocked, as this would freeze
the user interface.

