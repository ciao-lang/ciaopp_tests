% Experiment analysis configuration for bundles.

% all paths are relative to the bundle location.

priority_modules([]).  % prioritized module names in that order. Typically the
                       % entry point should be prioritized

% If missing, taken from test_dirs.pl
%% entry_module('cmds/main1.pl').
%% entry_module('cmds/main2.pl').

bundle_location('.'). % relative to ciao-devel/bndls

edit_dir(lib).    % directory of the sources to be tested
edit_dir(cmds).   % the dir that contains the entry point modules needs to be specified
% TODO: read src_dir from manifest