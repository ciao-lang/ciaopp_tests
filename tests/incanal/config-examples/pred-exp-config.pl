% Experiment configuration

% paths are relative to the location of this file, a subdirectory will be
% created in this directory
experiment_dir('test_results').      % (statistics, intermediate analyses, ...)

edition_mode(predicate).
% add/delete a full clause or predicate (not mixed for now)

module_permanence(1). % probability of staying in the same module after a change
predicate_permanence(1).
% probability of editing the same predicate after a change (only for clauses), not implemented yet

% valid if --rand is active
% probability of performing an addition/deletition action in a step
addition_probability(0.6). % must be greater than deletion if we want to finish the bundle
deletion_probability(0.6). % must be greater than addition if we want to remove the bundle
