(reset-handler (lambda () (exit 1)))
(parameterize ([cd ".."])
  (load "main.ss"))
