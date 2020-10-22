# Tests for incremental analysis

## Requirements
To run these tests `bash` is needed. 
To generate the plots `gnuplot` is needed.

## Organization	of source files:

The base organization is:

- `test_results/`   results for analysis of edition simulations.
- `bench/`	      benchmarks used for testing.

The benchmarks available are:

- aiakl
- ann
- bid
- boyer
- cleandirs
- hanoi
- peephole
- progeom
- prolog_read
- qsort
- rdtok
- warplan
- witt

## Compiling the tests
```shell
$ cd ~/ciao/ciaopp/src/plai/incanal/tests # test directory
$ ./compile.sh  # compile tests
```

## Running a subset of the experiments in the paper (faster)
**This takes 10-15 minutes.**

This will run the test for analyzing `shfr` benchmarks `boyer`,
`qsort`, and `peephole` and automatically open a pdf file for each of
set of experiments: addition and deletion.

```shell
$ ./quick_run.sh

Performing tests...
Bench options: shfr under_all --user_tag quick-run
Testing boyer for monolithic  ...
Logs are being printed in test_results/logs/boyer_add_monolithic_.log
Testing boyer for monolithic incremental ...
Logs are being printed in test_results/logs/boyer_add_monolithic_incremental.log
Testing boyer for modular  ...
Logs are being printed in test_results/logs/boyer_add_modular_.log
Testing boyer for modular incremental ...
Logs are being printed in test_results/logs/boyer_add_modular_incremental.log

...
```

Detailed time execution of the benchmarks (in the paper appear in the
appendix) can be found in `test_results/graphs/details_<date>`.

## Automatically generate graphs

Once the tests are finished you may automatically generate the runtime
execution graphs of the paper with script
`generate_results_summary.sh`. This script takes two parameters, the
first one to determine the results directory (fixed) and the second is
UNIX-like filter of which test results should appear in the
summary. When the process is finished, a pdf file is oppened with the
result.

### Examples:

Generate results for all the test performed for add simulation:

```shell
$ ./generate_results_summary.sh test_results "*add*"
```

Generate results summary for all the test performed for deletion
editions and domain `shfr`:

```shell
$ ./generate_results_summary.sh test_results "*del*shfr"
```

The summaries are stored at `./test_results/graphs/` in a file called
`statistics_<date>.pdf`. The detailed runtime execution of each state
of the analysis is stored at `./test_results/graphs/details_<date>`.

The performance obtained here should be proportional to the one seen
in the tables of the paper. Except for the loading phase, which could
be specially affected by the virtual environment.


## Automatically run one benchmark
```shell
$ cd  ~/ciao/ciaopp/src/plai/incanal/tests # test directory
$ ./run_configs.sh bench_name add/delete shfr/def [Opts]
```

This file leaves the results of a test in the directory
`test_results/bench_name-simulation_settings/incrementality_settings`.

If you want to re-run a test, this directory has to be manually **removed**.

Simulation settings are of the form:
`edit_type-not_rand-n_editions/iteration-abstract_domain-dd`. Each of
them mean:

- `edit_type`: whether the test consisted on adding or deleting.
- `n_editions/iteration`: number of clauses that were
  added/deleted each edition simulation.
- `abstract_domain`: abstract domain used for the analysis (any of the available: 
      `shfr`, `def`, or `gr`).
- `dd`: (internal) identifier of the fixpoint algorithm used to analyze
      (only `dd` is available for incremental analysis).
	  
The incrementality settings can be any of the studied configurations:

- For adding: `mon-noninc`, `mon-inc`, `mod-noninc`, and `mon-inc`.
- For deleting: `mon-noninc-td`, `mon-inc-td`, `mon-inc-bu`,
  `mod-noninc-td`, and `mon-inc-td`, `mod-inc-bu`.

Additionally, directory `test_results/logs`, contains the logs
of the tests executions.

### Example

Runing adding test for qsort bench:

``` shell
$ ./run_configs.sh qsort add shfr
Compiling...

Performing tests...
Testing qsort for monolithic  ...
Logs are being printed in test_results/logs/qsort_monolithic_.log

Testing qsort for monolithic incremental ...
Logs are being printed in test_results/logs/qsort_monolithic_incremental.log

Testing qsort for modular  ...
Logs are being printed in test_results/logs/qsort_modular_.log

Testing qsort for modular incremental ...
Logs are being printed in test_results/logs/qsort_modular_incremental.log

$ 
```

## Running one single test

The bench driver of single execution is `ciaopp-test` with the `incanal` option.

The program accepts several parameters: `ciaopp-test incanal bench_name add/del #changes/it domain [Opts]`

  * `bench_name`: name of the benchmark (one of the described earlier).
  * `edits/iteration`: number of changes (additions or deletions) that
      will be performed each iteration. In the paper we show analysis
      only for changes of one clause each time.
  * `domain`: domain for analysis. Available domains are sharing and
      freenes, def, and groundness (`shfr`/`def`/`gr`)
  * Opts are the approach configuration options, they can be written
    in any order:
	  * monolithic/modular:     Monolithic or modular analysis approach.
	  * incremental:            Fine-grain incrementality.
	  * `top_down`/`bottom_up`: Clause deletion strategy (described in the paper).
      * `--user-tag tag`:       An identifier for the future directory of the test
	  * `--start N`:            Number of simulated editions skipped.
	  * `--steps N`:            Number of editions simulated.

Note that if the parameters are	not the	expected ones the outcome may have errors.

### Examples

* Analyze `qsort` adding clauses 1 by 1 for sharing and freenes domain
with the modular incremental approach:
``` shell
$ cd ~/ciao/ciaopp/src/plai/incanal/tests
$ rm -rf test_results/qsort* # remove previous results (if existing)
$ ciaopp-test incanal qsort add 1 shfr modular incremental
```

* Analyze `aiakl` adding 3 clauses each iteration with groundness
  domain and the modular approach:

``` shell
$ rm -rf test_results/aiakl-* # remove previous results (if existing)
$ ciaopp-test incanal aiakl add 3 gr modular
```

* Analyze `bid` removing clauses 1 by 1 for sharing and freenes domain
with the monolithic incremental approach, with user tag "testing":
``` shell
$ rm -rf test_results/bid*
$ ciaopp-test incanal bid add 1 shfr monolithic incremental top_down not_rand --user_tag testing
```

* Analyze `cleandirs` adding 3 clauses each iteration with def domain with the modular incremental approach, starting in iteration 10 (e.g. from 30 clauses), and performing 5 steps (e.g. add 15 clauses):
``` shell
$ rm -rf test_results/cleandirs*
$ ciaopp-test incanal cleandirs add 3 def modular incremental --start 10 --steps 5
```

## Structure of the results directory

Each results directory contains:

* `*.pl` files with the last state of the edition simulation.
* `*.reg` containing the global analysis graph results of the last
  edition simulation splitted by module.
* Directory `detailed_step_results` which contains the
  results of each simulation step.
* Directory `stats` contains the statistics of each simulation
  step. These are used to build the graphs automatically by
  summarizing its information.
* Directories of the form `state_X` have the module that was modified
  with respect to `state_X-1`. The initial state of the simulation is
  placed in `state_0`.


## Interpreting the logs

Each time the files are simulated to be edited, a display with
*# Edition iteration X* is displayed. Then, each time the global answer
table is updated (see algorithm in paper), we display it in the log.

Example, the first iteration of
``` shell
$ ciaopp-test incanal qsort add 1 shfr modular incremental
```

```shell
Global answer table for [shfr]
 Pred | SubGoal | Call | Success
------|---------|------|---------
qsort:qsort/2 | qsort:qsort(A,B) | [[A],[A,B],[B]],[A/nf,B/nf] | [[A],[A,B],[B]],[A/nf,B/nf]
mylists:length/2 | mylists:length(A,B) | [[A],[B]],[A/f,B/nf] | [[B],[B,A],[A]],[B/nf,A/nf]
mylists:length/2 | mylists:length(A,B) | [[A],[B]],[A/nf,B/f] | [[A],[A,B],[B]],[A/nf,B/nf]
test:test/1 | test:test(A) | [[A]],[A/nf] | $bottom
------|---------|------|---------}
```

Variables always begin with a capital letter.

Columns represent:

* Pred: module:rule/arity, the rule to which the info refers to.
* SubGoal: how the variables used in the substitutions are named.
* Call: call pattern for that entry.
* Success: success pattern for that entry.

In the sharing and freeness domain, intuitively, two variables
are in the same list if they may share, singletons mean that there may
also be other non-shared variable. Freeness is represented with Var/State.
State can be: `nf` meaning non-free, `f` meaning free and `g` meaning ground.

For example, tuple: `mylists:length/2 | mylists:length(A,B) |
[[A],[B]],[A/f,B/nf] | [[B],[B,A],[A]],[B/nf,A/nf]` is interpreted
as rule length with arity 2 of module mylists is called with a call
pattern in which A and B are not shared, A is a free variable and B is
a non-free variable. If the rule succeeds, B and A are shared and they
both are non-free variables.
