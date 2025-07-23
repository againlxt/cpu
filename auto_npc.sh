#!/bin/bash
sbt clean compile run
rm ~/ysyx-workbench/npc/vsrc/npc_cpu/*
mv ./generated/* ~/ysyx-workbench/npc/vsrc/npc_cpu
cp ./src/main/verilog/* ~/ysyx-workbench/npc/vsrc/npc_cpu
