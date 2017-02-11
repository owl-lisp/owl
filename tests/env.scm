(import (owl sys))

(print (getenv "FOO"))
(setenv "FOO" "BAR")
(print (getenv "FOO"))
