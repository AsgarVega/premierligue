#!/bin/bash
docker build -t prolog .
docker run -it --rm -p 8000:8000 --name example prolog swipl