:- module(_,[],[assertions]).

:- doc(filetype, documentation).

% TODO: document the difficulties of each of the benchmarks

% TODO: move this to lpdoc syntax

% In the following editing means adding, deleting and modifying (expressed in
% terms of adding and deleting syntactically).
%
% All the following benchmarks are git repositories and may contain more than
% one exported predicate:

% * `trust_calls`: basic call assertions in exported predicates (special case).
% * `trust_calls_complex`: basic call assertions not in exported predicates.
% * `trust_success`: basic success assertions in exported predicates.
%
% To check all the cases in applying call assertions incrementally we have:
% Options regarding editing call assertions:
%  1. (C1) It changes the call of a complete and no complete is found that matches the new CP. 
%  2. (C2) It changes the call of a complete and there is a complete for that call.
%  3. (C3) It does not change the result of a **complete**.
%
% Options regarding the how the complete substitution:
%  1. (+) The complete is more general.
%  2. (-) The complete is more specific.
%
% The benchmark names are organized following the notation of the conditions
% above: `trust_calls_<CX>_<+/->`. E.g. the benchmark that tests that ``editing an
% assertion causes the call pattern to grow and there is no previous complete
% for that CP'' would be `trust_calls_C1_+`.
%

% Assertions not in exported predicates:
% S1: an assertion is added but it is not applicable
% S2: an assertion is removed but it was not applicable
% S3: an assertion is added & exactly applicable (combined with +/-)
% S4: an assertion is removed & exactly applicable (combined with +/-)
