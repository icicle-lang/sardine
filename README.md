sardine
=======

Thrift IDL compiler and runtime for encoding/decoding thrift compact format.


### Usage

```shell
$ sardine foo.thrift
```

### Calling from build system

Not sure how we'll do this yet? Maybe we don't need to?

We have a few options:

- Use a master runner
- Check in generated code


### What's wrong with the official Thrift library on Hackage?

#### Union types

It doesn't support union types in any sensible way. For example, the
Ivory `ThriftFactPrimitiveValue` union type is represented as follows:

```haskell
data ThriftFactPrimitiveValue =
  ThriftFactPrimitiveValue {
      thriftFactPrimitiveValue_s :: LT.Text
    , thriftFactPrimitiveValue_i :: I.Int32
    , thriftFactPrimitiveValue_l :: I.Int64
    , thriftFactPrimitiveValue_d :: P.Double
    , thriftFactPrimitiveValue_b :: P.Bool
    , thriftFactPrimitiveValue_t :: ThriftTombstone
    , thriftFactPrimitiveValue_date :: I.Int32
    } deriving (Show,Eq,Generic,Typeable)
```

Each field is pre-filled with a default value (`""`, `0`, `0.0`,
`False`, etc) so it's impossible to figure out which field is set.

#### Partial functions

Almost all of the functions which can fail just call `error` if there is
a problem rather than capturing possible errors in the type. One example
of this is when decoding enums, it pulls a `Word8` off the wire, and
passes it straight to `Enum(toEnum)` for the type in question.
