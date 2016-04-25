CTH Colorize
============

Common test hook to output each status of each testcase.


Usage:
```
ct_run -ct_hooks cth_colorize -pz cth_colorize/_build/default/lib/*/ebin/ test/
```

Example output:

```
Testing git.matstat: Starting test, 17 test cases
 * matstat_SUITE:mean                                                     [ OK ]
 * matstat_SUITE:mean_stddev                                              [ OK ]
 * matstat_SUITE:hmean                                                    [ OK ]
 * matstat_SUITE:gmean                                                    [ OK ]
 * matstat_SUITE:tmin                                                     [ OK ]
 * matstat_SUITE:tmax                                                     [ OK ]
 * matstat_SUITE:tvar                                                     [ OK ]
 * matstat_SUITE:tstd                                                     [ OK ]
 * matstat_SUITE:tsem                                                     [ OK ]
 * matstat_SUITE:cmedian                                                  [ OK ]
 * matstat_SUITE:linregress                                               [ OK ]
 * matstat_SUITE:itemfreq                                                 [ OK ]
 * matstat_SUITE:pearsonsr                                                [ OK ]
 * matstat_SUITE:moment                                                   [ OK ]
 * matstat_SUITE:skewness                                                 [ OK ]
 * matstat_SUITE:kurtosis                                                 [ OK ]
 * matstat_SUITE:histogram                                                [ OK ]
Testing git.matstat: TEST COMPLETE, 17 ok, 0 failed of 17 test cases
```
