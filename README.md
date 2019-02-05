# cl-resources-plantuml-generator

Generates a UML diagram from your mu-cl-resources configuration by
simple docker command.

## Example use

Move into your mu-project and execute the following:

    docker run \
      --rm \
      -v `pwd`/config/resources:/config \
      -v `pwd`/tmp:/config/output/ \
      madnificent/cl-resources-plantuml-generator

Once completed, behold the contents of the tmp folder.

## Current use

In practice, most of the API space of an application is covered by
mu-cl-resources.  This component describes much of the semantic model
and much of the domain the frontend can access.  A clear overview of
the available resources can help.  This component makes it easy to
srender a visual overview of the available resources.

## Future

This PoC allows us to see the value of UML diagrams for components we
often use.  The current vision is to extend this support to various
other microservices and enrich the SVG so more in-depth information
can be rendered interactively.
