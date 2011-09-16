%% ParZu PARAMETER FILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%modus(fileout).    % output to file, batch, without showing ASCII parsetrees
modus(showparses).		% show ASCII parsetree

%turning statistics off leads to very poor results (the grammar is very lax, because the statistics module filters out some impossible results)
statmod(on).
% statmod(off).

ambi(2).	                % number of readings returned minus 1. minimally 2.

%% NEW speed parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constant(levels,20).             % CYK levels. default is 30, use vals between 10 and 30.
constant(alter,2).               % number of alternatives for same stretch at normal pruning. default is 5, use vals between 2 and 6.
constant(alterlocal,1). 	 %discard locally if not among alter + alterlocal best alternatives. Prunes earlier than normal pruning. Set to -1 to disable. (Have made best experience with 1).
constant(aggressive_start, 4096). % number of chart entries from where aggressive pruning starts. default is 500
constant(aggressive_thresh,0.03).% treshold below which to cut at aggressive pruning. default is 0.01
constant(commit,1.05).           % early commitment locally if above this probability: do not search for alternatives (=deterministic)
constant(discard,0.05).          % discard locally if below this probability. default is 0.05


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Usage example:
%% tell('6506_OUT.res'), seen, go_textual('suste-prologfacts_morphed5.pl'), told.
