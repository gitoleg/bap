# BAP tracing library

This library gives a program interface to a program trace. It doesn't
define a format in which a trace is stored and doesn't rely on a
particular tracer as a back end. It defines an interface to a trace
data structure per ce, and also specifies an interface for a backend,
so that a user can add support for new tracing tools and data formats.

A trace is represented as a sequence of events accompanied with a
dictionary of meta information. Since sequence is lazy by its nature,
we can represent both dynamic (aka interactive) tracing tools, and
static. Even for static tracing tools this is useful, as it allows us
not to load the whole trace at once.

Events, as well as meta attributes are represented with universal
values, so that trace can essentially contain any information. Since
BAP's universal values are serializable, we naturally get a first data
format for free. And we also propose this format to be used as default
one, or a as a lingua franca, that can be used to translate between
other formats.

# Roadmap

1. Implement the `Bap_trace` module and fill all `failwith "unimplemented"` gaps.

   This is fairly straightforward.


2. Implement protobuf reader protocol, so that we can read BAP legacy traces

3. Implement binprot readers and writers.
