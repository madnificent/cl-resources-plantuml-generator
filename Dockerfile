FROM semtech/mu-cl-resources:1.22.2

COPY . /app/dependencies/resources-openapi-generator/
COPY ./startup.lisp /usr/src/startup.lisp

RUN cd /; wget -O plantuml.jar https://github.com/plantuml/plantuml/releases/download/v1.2023.12/plantuml-1.2023.12.jar

RUN apt-get update; apt-get -y upgrade;
RUN apt-get -y install default-jre-headless graphviz

COPY ./start-generator.sh /start-generator.sh

CMD ["/start-generator.sh"]
