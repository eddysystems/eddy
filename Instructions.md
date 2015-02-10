### What is eddy?

Eddy is a plugin for IntelliJ that helps you write code faster. It can auto-correct
broken Java to legal Java, correcting syntax as well as many common semantic errors.
Even considering IntelliJ's pretty advanced inspection and correction features, you
will save a lot of time. Consider this input:

> TODO

IntelliJ highlighted an error, and you can hover over the line to find out what's
wrong. However, it won't offer to fix the problem, and correcting the mistakes requires
time and breaks your workflow.

Eddy on the other hand offers a solution immediately:

> TODO

Just hit Alt+Enter to accept it and continue working.

Eddy seamlessly integrates into IntelliJs user interface, and uses common shortcuts
that you're already used to.

But that's not all. With eddy, you can start writing shorthand, as long as it's
reasonably clear what you're trying to do. Eddy knows shorthands for many common
language constructs, for instance:

> for i in tokens

Eddy also does not insist on the details of Java syntax. For example, If you're
used to writing C++, you may be tempted to write

> Arrays.copyOf<T,U>(a, n, b)

instead of

> Arrays.<T,U>copyOf(a, n, b)

Eddy will sort this out for you. Eddy is a learning system, and there's no specific
set of things that it understands. Our philosophy is that if it was clear what you
are trying to say, Eddy should understand it. If eddy doesn't understand something,
you should [leave a suggestion](TODO).

### Installing eddy

Eddy is plugin for the excellent [IntelliJ IDEA](TODO) Java IDE. It is compatible
with IntelliJ Idea 13 and 14. To install it, install IntelliJ if you don't have it
already, and then go to `Preferences / Plugins` and click `Browse Repositories`.
You can search for "eddy" or select it from the list. After a restart, eddy is
installed.

Eddy scans projects every time they're opened, a process that takes around 10-15
seconds. Of course, you can work while eddy is initializing in the background,
and eddy should be available even while it is still initializing.

### Interacting with eddy

We made it a priority to integrate as cleanly as possible into IntelliJ's user
interface. If you're used to working with IntelliJ, eddy will feel natural to you.

Eddy is only active in Java editor windows, and only inside of code blocks. It
does not currently offer suggestions for class, method, or field declarations,
or for import or package statements.

Whenever eddy is active and you move to a new line of code or type something,
eddy will process the current line of code (or, to be exact, the current line
or the current statement, whichever is larger). This happens in the background
and should not interfere with your editing. If eddy finds a solution, it will
offer an [intention action](TODO). If eddy finds only solutions that are
different than what you wrote, it also shows a [hint](TODO).

## Eddy hints



## Eddy's intention action

## Suggesting a solution for eddy

### Configuring eddy
