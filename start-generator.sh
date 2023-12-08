#!/bin/bash
echo "Making directory"
sh -p mkdir /config/output
echo "Loading config thtrough load-config.sh"
sh /load-config.sh
echo "Loading startup.lisp through sbcl"
sbcl --load /usr/src/startup.lisp
echo "Changing to root folder"
cd /
echo "Running plantuml generator"
java -jar plantuml.jar -tsvg /config/output/jsonapi-domain.plantuml
