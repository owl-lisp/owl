(import (owl sys))
(print (filter m/^dir\.scm$/ (dir->list "tests")))
