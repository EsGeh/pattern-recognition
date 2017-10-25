#!/bin/bash

stack build && stack test :nearestNeighbours-test

# stack test --test-arguments="O2" :neuralNetworks-test 2>&1 | tee plots/log
