# How to use
```
(ql-dist:install-dist "https://github.com/snmsts/mirror-dist/releases/download/distinfo/distinfo.txt")
```
# https patch (for people who doesn't use roswell)
You might need to patch quicklisp before use this dist.Original quicklisp does not have capability to download from https uris and github releases does not have http serving functionality.
my choice is to patch quicklisp. Quicklisp setup via roswell already have https access patch you don't need it.[*](https://github.com/quicklisp/quicklisp-projects/issues/1315)
```
$ curl -O https://raw.githubusercontent.com/snmsts/mirror-dist/main/scripts/https-support.lisp
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  1650  100  1650    0     0   4301      0 --:--:-- --:--:-- --:--:--  4296
$ sbcl --load https-support.lisp
This is SBCL 2.3.4, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
patch applied to /**/**/quicklisp/local-init/https.lisp
$
```

# toml format and people who'd like to clone and have your own dists
TBD so far I recommend [ultralisp registration](https://ultralisp.org/). 
