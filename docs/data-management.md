# Data representation

Icepeak is a key-value store for JSON data. In memory, the state is represented
as a `Data.Aeson.Value`, on disk, it is serialized as the textual JSON
representation.

# Modifications

Icepeak supports two kinds of modification operations:

- `Put` writes a JSON value to a given path in the JSON structure.

  - If the path descends into a JSON value that is not an object, that value is
    replaced by an object that just contains the newly created path.
  - If a value along the path is already an object, the field specified by the
    path is added to it.

- `Delete` removes a JSON value at a given path.

  - If the path does not exist, nothing happens.
  - If the path refers to a field in an object, that field is removed from the
    object.

Note that in the current implementation, HTTP requests to the REST API return
before the modification has been applied to the data. This is because updates
are delegated to a different thread than the HTTP request.

# Atomic writes

To make sure an interrupted write does not corrupt the database, the new value
is first written to a file with the same name as the data file but with the
extension `.new` appended. For example, if the data is stored in `icepeak.json`,
the new data will first be written to `icepeak.json.new`.

Only then, `icepeak.json.new` is renamed to `icepeak.json`, thereby overwriting
it. POSIX guarantees that this rename operation is atomic.

# Journaling

Icepeak optionally supports journaling in order to prevent data loss.

Every operation is written to the journal before it is applied to the in-memory
representation. Whenever the in-memory value has been persisted to disk, the
journal is cleared.

In the case of a hard crash, Icepeak uses the journal on the next startup to
replay any operations that have not yet been persisted in the on-disk value.

## Journal file format

The journal is held in a file with the same name as the data file but with
`.journal` appended. For example, if the data is stored in `icepeak.json`, the
journal is kept in `icepeak.json.journal`.

The journal is a simple text based format where every line (separated by `\n`)
represents one operation, encoded in JSON without any unnecessary whitespace. In
particular, the JSON representation of an operation does not contain new line
characters. New line characters occurring the serialized text are escaped.

The two kinds of operations are encoded as follows:

- A `Put` operation writing the JSON value `someval` to the path `somepath` is encoded as

  ```
  {"op":"put","path":somepath,"value":someval}
  ```

- A `Delete` operation deleting the path `somepath` is encoded as

  ```
  {"op":"delete","path":somepath}
  ```

The paths are encoded as JSON arrays of the individual components. The path
`/this/is/a/path` is encoded as `["this","is","a","path"]`. The root path is
represented by the empty array `[]`.
