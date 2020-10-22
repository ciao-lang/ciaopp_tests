% Experiment configuration

% paths are relative to the location of this file, a subdirectory will be
% created in this directory
results_dir('.').      % (statistics, intermediate analyses, ...)

edition_mode(predicate).
% add/delete a full clause or predicate (not mixed for now)

% TODO: not implemented using absolute number of clauses or % of the program

module_permanence(0.5). % probability of staying in the same module after a change
predicate_permanence(0.5). % probability of editing the same predicate after a change

% probability of performing an addition/deletition action in a step
addition_probability(0.6). % must be greater than deletion if we want to finish the bundle
deletion_probability(0.6). % must be greater than addition if we want to remove the bundle

% TODO: code mutations are not considered here (change atoms or reorder literals)