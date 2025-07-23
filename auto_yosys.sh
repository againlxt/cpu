#!/bin/bash
sbt clean compile run
rm ~/ysyx-workbench/npc/vsrc/yosys/*
mv ./generated/* ~/ysyx-workbench/npc/vsrc/yosys
