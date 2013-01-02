#!/bin/bash

echo -e " * Getting problem list"
curl http://localhost:3000/problems

echo -e "\n\n * Getting ModusPonens problem"
curl http://localhost:3000/problem/ModusPonens

echo -e "\n * Posting incorrect solution"
curl -X POST --data-binary foo http://localhost:3000/solve/ModusPonens

echo -e "\n\n * Posting solution"
curl -X POST --data-binary @ModusPonens-solution.agda http://localhost:3000/solve/ModusPonens

echo -e "\n\n * Tests done"

