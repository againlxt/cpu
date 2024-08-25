#!/bin/bash
sbt clean compile run
rm ~/ysyx-workbench/npc/vsrc/single_cycle_cpu/*
mv ./generated/* ~/ysyx-workbench/npc/vsrc/single_cycle_cpu
