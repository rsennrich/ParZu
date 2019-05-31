#!/bin/sh

#example call (given the training data in a conll format):
#./create_statistics.sh tuebadz-10.0-conll2010.txt

if [ $# -lt 1 ]
then
    echo "Error in $0 - Invalid Argument Count"
    echo "Syntax: $0 tüba_file conll10 format"
    exit
fi

mkdir tmp

python stats_creator/conll2prolog.py word < $1 > tmp/train.pl

# do some conversion before stats extraction (mostly attaching verb particles to full verbs and making sure that we consider the full verb for relation counts).
swipl -s stats_creator/ptkvz.pl -t "start('tmp/train.pl', 'tmp/train_ptkvz.pl')."
swipl -s stats_creator/goldstandard_convert.pl -t "start('tmp/train_ptkvz.pl', 'tmp/train_raised.pl', raise)."
swipl -s stats_creator/goldstandard_convert.pl -t "start('tmp/train_raised.pl', 'tmp/train_swapped.pl', swap)."
swipl -s stats_creator/goldstandard_convert.pl -t "start('tmp/train_swapped.pl', 'tmp/train_raised2.pl', raise)."
cat tmp/train_raised2.pl | awk '{print tolower($0)}' > tmp/train_stats.pl

swipl -G1024M -L1024M -s stats_creator/verbstatsextracter.pl -t "start('tmp/train_stats.pl', 'tmp/vstat_data_nolemma.pl')."
swipl -G1024M -L1024M -s stats_creator/ppstatsextracter.pl -t "start('tmp/train_stats.pl', 'tmp/ppstat_data_nolemma.pl', 'tmp/freq_data_nolemma.pl')."
#disabled for now: TüBa 7 data introduces an undesirable bias
#swipl -G1024M -L1024M -s stats_creator/konjstatsextracter.pl -t "start('tmp/train_stats.pl', 'tmp/konjstat_data.pl')."
swipl -G1024M -L1024M -s stats_creator/advstatsextracter.pl -t "start('tmp/train_stats.pl', 'tmp/advstat_data_nolemma.pl')."
swipl -G1024M -L1024M -s stats_creator/adv_predstatsextracter.pl -t "start('tmp/train_stats.pl', 'tmp/adv_pred_data_nolemma.pl')."
swipl -G1024M -L1024M -s stats_creator/gmodstatsextractor.pl -t "start('tmp/train_stats.pl', 'tmp/gmod_ne_data.pl')."

python stats_creator/conll2prolog.py lemma < $1 > tmp/train_lemma.pl

# do some conversion before stats extraction (mostly attaching verb particles to full verbs and making sure that we consider the full verb for relation counts).
swipl -s stats_creator/ptkvz.pl -t "start('tmp/train_lemma.pl', 'tmp/train_ptkvz_lemma.pl')."
swipl -s stats_creator/goldstandard_convert.pl -t "start('tmp/train_ptkvz_lemma.pl', 'tmp/train_raised_lemma.pl', raise)."
swipl -s stats_creator/goldstandard_convert.pl -t "start('tmp/train_raised_lemma.pl', 'tmp/train_swapped_lemma.pl', swap)."
swipl -s stats_creator/goldstandard_convert.pl -t "start('tmp/train_swapped_lemma.pl', 'tmp/train_raised2_lemma.pl', raise)."
cat tmp/train_raised2_lemma.pl | awk '{print tolower($0)}' > tmp/train_stats_lemma.pl

swipl -G1024M -L1024M -s stats_creator/verbstatsextracter.pl -t "start('tmp/train_stats_lemma.pl', 'tmp/vstat_data.pl')."
swipl -G1024M -L1024M -s stats_creator/ppstatsextracter.pl -t "start('tmp/train_stats_lemma.pl', 'tmp/ppstat_data.pl', 'tmp/freq_data.pl')."
swipl -G1024M -L1024M -s stats_creator/advstatsextracter.pl -t "start('tmp/train_stats_lemma.pl', 'tmp/advstat_data.pl')."
swipl -G1024M -L1024M -s stats_creator/adv_predstatsextracter.pl -t "start('tmp/train_stats_lemma.pl', 'tmp/adv_pred_data.pl')."
