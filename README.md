# SchemaViz

Visualizes TGraph Schemas using GraphViz.

## Getting started

This project uses [Leiningen](https://github.com/technomancy/leiningen) for
retrieving all its dependencies from various Maven repositories, building, and
test automation.

Getting started is really simple:

- Install the `lein` shell (or bat) script as explained at the
  [Leiningen homepage](http://leiningen.org) page.

- Fetch the project's dependencies:

```
$ cd schemaviz
$ lein deps
```

- Visualize a schema to a PDF document:

```
$ lein run my-schema.tg my-schema.pdf
```

- There are also some options you may provide to change how things look like.
  To get a list of supported options, just run:

```
$ lein run
Usage:
======

lein run [options] schemafile.tg outfile

Valid options are:
--edgeclass-weight: The weight of edgeclass edges.  If it is
  higher than :specialization-weight, then edgeclass edges are tried to be
  shorter and more straight than specialization edges.
  Default value: 1
...
```

## License

Copyright (C) 2012 The JGraLab Team <ist@uni-koblenz.de>

Distributed under the
[General Public License, Version 3](http://www.gnu.org/copyleft/gpl.html).

<!-- Local Variables:        -->
<!-- mode: markdown          -->
<!-- indent-tabs-mode: nil   -->
<!-- End:                    -->

