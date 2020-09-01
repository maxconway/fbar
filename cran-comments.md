## Test environments
- elementary OS 5.1.7 Hera, built on Ubuntu 18.04 LTS, R 4.0.2
- travis-ci:
  - ubuntu 16.04, oldrel
  - ubuntu 16.04, release
  - ubuntu 16.04, devel
- win-builder:
  - oldrel
  - release
  - devel
- rhub

## R CMD check results

0 errors | 0 warnings

- 1 NOTE, somtimes:

I get the following note when testing on some platforms.
The upvoted answer on stack overflow says that happens when http://worldclockapi.com/api is down.
At the time of submission this appears to be the case, with a 403 error, so I am assuming it is out of my hands.

```
checking for future file timestamps ... NOTE
  unable to verify current time
```

## Reverse dependencies

None.
