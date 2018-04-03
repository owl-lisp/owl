(import (owl sys))

(setenv "FOO" "BAR")
(print (getenv "FOO"))
(unsetenv "FOO")
(print (getenv "FOO"))
