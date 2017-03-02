Collaborative WYSIWYG Editor
============================

The aim of this project is to provide an online, collaborative WYSIWYG
editor which also supports independent actors working simultaneously
offline for periods of time. When they reconnect, their work needs to
be merged automatically.

There are several major issue which must be dealt with.

 1. the underlying mechanisms of collaboration and merging

 2. the hassles of creating a pleasant editor experiencing using
 HTML, CSS, and GHCJS.

To retain our sanity, we need a good core abstraction for modeling
the editor.

The editor is intended to remotely edit a `Document` which resides on
a centralized server. A `Document` is simply an ordered sequence of
`Patch Atom`. All edits to the `Document` are done by appending a new
`Patch`. The `Document` is append-only. Once patches are appended,
they are never mutated or reordered.

One or more clients can edit the same `Document` at the same
time. This is done by sending proposed patches to the server. The
server will merge the patch (which may mutate it), and send the
updated patch to all connected clients, including the client that
original sent the patch. The merge algorithm that the server uses
always succeeds. If the patch contains a conflict, this will be
explicitly encoded in the patch that is returned.

Although not currently implemented it would be trivial for a patch to
contain additional metadata such as author name and timestamp. This means
that essentially every keystroke that the user enters is recorded and
can be replayed.

A `Patch` is merely a list of `Edit`. An `Edit` either inserts,
deletes, or replaces a token. In the editor a token type is `Atom`.

An `Atom` is the smallest unit that can be added to a
document. `Atoms` are things like a character, and image, a
linebreak. For efficiency sake, it also includes things like
`RichText`.

On the client-side we will have a local copy of the `Document` known
as `LocalDocument`. The `LocalDocument` contains a (possibly out of
date) copy of the `Document` plus any changes that the user has made
locally.

In the `LocalDocument` we need to track a few pieces of information:

 1. The `Document` that we forked from. This includes the patch number.

 2. Patches we have created locally

 3. Patches we have sent to the server, but not yet heard back about

 4. The Patch that is currently in progress.

We could create a new patch for every single keystroke, mouse
click, etc. However, it would be inefficient. A `Patch` is just a list
of `Edit`, and so we instead group a bunch of edits together. The
exact algorithm for grouping edits will require some fine tuning. In
theory a `Patch` could contain an arbitrarily large number of
`Edits`. But, when we have multple clients editing the same
`Document`, the changes will only be propogated when the client
submits the `Patch`. So we want the `Patch` granularity to be small
enough that it gives the feeling of realtime collaboration.

A starting metric would be to create a new patch anytime the user does
one of the following:

 1. presses the return key

 2. presses the backspace/delete key

 3. clicks on something

 4. is idle for more than 1 second

 5. loss of focus, page close, etc

Server <-> Client Communication
-------------------------------

Because this is a multi-user editor we need bidirection
communication. The server can send updates to the client at any time
without provocation. This is implemented using websockets. More on
that later.

There are a couple situations we need to handle. The server could send
is a new patch at anytime. That means we need to be able to handle
merges on the client side, since the patch might conflict with edits
and patches we have not sent yet.

The client could also become disconnected from the server temporarily
-- either accidentally or on purpose (offline mode). So we need to
make use of localStorage to ensure that the patches are not going to
be lost.

The primary reason for making patches small is so that when multiple
user are editting the same document you get realtime feeling rather
than giant chunks of changes.

Since the server knows if there is more than one client, it could vary
chunk sizes dynamically.

So, this fomulation of `LocalDocument` makes sense:

    data LocalDocument = LocalDocument
      { _upstreamDocument :: Document
      , _inflightPatch    :: Maybe Patch
      , _currentEdit      :: [Edit Atom]
      }

The `_inflightPatch` is the one we are currently sending (or trying to
send) to the server. We can continue to keep the `_currentEdit` open
until we receive the applied patch back from the server.