FROM semtech/mu-cl-resources

COPY . /app/dependencies/resources-openapi-generator/
ADD ./startup.lisp /usr/src/startup.lisp

RUN cd /; wget https://kent.dl.sourceforge.net/project/plantuml/plantuml.jar
RUN apt-get update; apt-get -y upgrade;
RUN apt-get -y install default-jre-headless graphviz

CMD sh /load-config.sh; sh mkdir /config/output; sbcl --load /usr/src/startup.lisp; cd /; java -jar plantuml.jar -tsvg /config/output/jsonapi-domain.plantuml
