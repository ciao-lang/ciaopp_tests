% Experiment configuration for bundle 'ciaofmt' (sample).

% all paths are relative to the bundle location.

priority_modules([ciaofmt]).  % prioritized module names in that order. Typically the
                       % entry point should be prioritized

entry_module('cmds/ciaofmt.pl'). % TODO: this should be in the manifest

bundle_location('.'). % relative to ciao-devel/bndls

edit_dir(lib).    % directory of the sources to be tested
edit_dir(cmds).   % the dir that contains the entry point modules needs to be specified
% TODO: read src_dir from manifest
