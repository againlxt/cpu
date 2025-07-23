#!/bin/bash
sbt clean compile run
rm ~/ysyx-workbench/npc/vsrc/soc_cpu/*
mv ./generated/* ~/ysyx-workbench/npc/vsrc/soc_cpu
cp ./src/main/verilog/* ~/ysyx-workbench/npc/vsrc/soc_cpu
