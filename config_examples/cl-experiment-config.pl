% Edition experiment configuration

% paths are relative to the location of this file, a subdirectory will be
% created in this directory
experiment_dir('test_results').  % (statistics, intermediate analyses, ...)

edition_mode(clause).
% add/delete a full clause or predicate (not mixed for now)

module_permanence(1). % probability of staying in the same module after a change
% predicate_permanence(1). % NOT IMPLEMENTED
% probability of editing the same predicate after a change (only for clauses), not implemented yet

% valid if --rand is active
% probability of performing an addition/deletition action in a step
add_probability(0.6). % must be greater than deletion if the bundle is to be finished
del_probability(0.6). % must be greater than addition if the bundle is to be removed
