(import
   (only (owl sys) setenv unsetenv)
   (scheme process-context))

(setenv "FOO" "BAR")
(print (assoc "FOO" (get-environment-variables)))
(print (get-environment-variable "FOO"))
(unsetenv "FOO")
(print (get-environment-variable "FOO"))
