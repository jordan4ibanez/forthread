# forthread
A threading library for Fortran.

This is an extremely simple binding to POSIX pthread.

It comes with two useful components:

- A thread pool/dispatcher.
- A concurrent dense FIFO linked queue.

[Please see the tutorial](https://github.com/jordan4ibanez/forthread/blob/master/test/example.f90)

### Add to your project:

In your fpm.toml add:

```toml
[dependencies]
forthread = { git = "https://github.com/jordan4ibanez/forthread" }
```

Like what I do? [My Patreon](https://www.patreon.com/jordan4ibanez)
