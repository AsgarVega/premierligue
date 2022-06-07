#!/bin/bash
docker run -it --rm -p 8000:8000 -v /home/opc/premierligue/app:/app --name example swipl:stable bash