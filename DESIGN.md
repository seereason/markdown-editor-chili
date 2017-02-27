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

 3. Patches we have sent to the server, but not yet heard back

 4. The Patch that is currently in progress.

