#!/bin/bash
sbt clean compile run
rm ~/ysyx-workbench/npc/vsrc/bus_cpu/*
mv ./generated/* ~/ysyx-workbench/npc/vsrc/bus_cpu
