(import (owl sys))

(setenv "FOO" "BAR")
(print (getenv "FOO"))
(setenv "FOO" #false)
(print (getenv "FOO"))
