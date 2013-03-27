#!/bin/sh

swipl -t "qcompile(gmod_ne_data),qcompile(adv_pred_data),qcompile(adv_pred_data_nolemma),qcompile(advstat_data),qcompile(advstat_data_nolemma),qcompile(konjstat_data),qcompile(freq_data),qcompile(freq_data_nolemma),qcompile(ppstat_data),qcompile(ppstat_data_nolemma),qcompile(vstat_data),qcompile(vstat_data_nolemma)."

